import html
import json
import re
import time
import urllib.parse
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
BIB_PATH = ROOT / "publications.bib"
REPORT_PATH = ROOT / "files" / "publications_enrichment_report.md"
RAW_PATH = ROOT / "files" / "publications_enriched.json"

ALLOWED_TOPICS = [
    "personalized-medicine",
    "reinforcement-learning",
    "random-forests",
    "survival-analysis",
    "dimension-reduction",
    "nutrition-science",
    "influenza",
    "sepsis",
    "others",
]


def fetch_text(url: str, timeout: int = 20) -> str:
    req = urllib.request.Request(
        url,
        headers={
            "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"
        },
    )
    with urllib.request.urlopen(req, timeout=timeout) as response:
        charset = response.headers.get_content_charset() or "utf-8"
        return response.read().decode(charset, errors="replace")


def parse_bib_entries(text: str):
    entries = []
    blocks = re.findall(r"@misc\s*\{\s*([^,]+),([\s\S]*?)\n\}", text, flags=re.IGNORECASE)
    for key, body in blocks:
        fields = {}
        for name, value in re.findall(r"(\w+)\s*=\s*\{([\s\S]*?)\}\s*,?", body):
            fields[name.lower()] = re.sub(r"\s+", " ", value).strip()
        entries.append({"key": key.strip(), "fields": fields})
    return entries


def parse_keywords(keyword_text: str):
    result = {"track": "method-theory", "venue": "journal", "topics": []}
    for part in keyword_text.split(";"):
        p = part.strip()
        if p.startswith("track:"):
            result["track"] = p.split(":", 1)[1].strip().lower()
        elif p.startswith("venue:"):
            result["venue"] = p.split(":", 1)[1].strip().lower()
        elif p.startswith("topics:"):
            topics = p.split(":", 1)[1].strip()
            result["topics"] = [t.strip().lower() for t in topics.split("|") if t.strip()]
    return result


def normalize_venue(v: str):
    v = (v or "").lower().strip()
    if v in {"preprint", "journal", "conference"}:
        return v
    if "conference" in v:
        return "conference"
    if "preprint" in v:
        return "preprint"
    return "journal"


def normalize_url(url: str):
    if not url:
        return url
    u = url.strip()
    m = re.search(r"arxiv\.org/pdf/([0-9]{4}\.[0-9]{4,5})(?:v\d+)?\.pdf", u, flags=re.IGNORECASE)
    if m:
        return f"https://arxiv.org/abs/{m.group(1)}"
    return u


def parse_meta(content: str, names):
    for n in names:
        m = re.search(
            rf'<meta[^>]+(?:name|property)=["\']{re.escape(n)}["\'][^>]+content=["\']([^"\']+)["\']',
            content,
            flags=re.IGNORECASE,
        )
        if m:
            return html.unescape(m.group(1).strip())
    return ""


def parse_meta_authors(content: str):
    authors = []
    for m in re.findall(
        r'<meta[^>]+(?:name|property)=["\'](?:citation_author|dc\.creator|author|parsely-author)["\'][^>]+content=["\']([^"\']+)["\']',
        content,
        flags=re.IGNORECASE,
    ):
        a = html.unescape(m.strip())
        if a and a not in authors:
            authors.append(a)
    return authors


def parse_crossref_authors(items):
    if not items:
        return []
    top = items[0]
    out = []
    for a in top.get("author", []):
        given = (a.get("given") or "").strip()
        family = (a.get("family") or "").strip()
        if given or family:
            out.append((given + " " + family).strip())
    return out


def fetch_crossref_metadata(title: str):
    q = urllib.parse.quote(title)
    url = f"https://api.crossref.org/works?query.title={q}&rows=1"
    raw = fetch_text(url)
    data = json.loads(raw)
    items = data.get("message", {}).get("items", [])
    if not items:
        return None
    top = items[0]
    title_val = ""
    if top.get("title"):
        title_val = top["title"][0]
    abstract = re.sub(r"<[^>]+>", " ", top.get("abstract") or "")
    return {
        "source": "crossref",
        "title": re.sub(r"\s+", " ", title_val).strip(),
        "abstract": re.sub(r"\s+", " ", abstract).strip(),
        "authors": parse_crossref_authors(items),
        "url": top.get("URL", ""),
    }


def fetch_page_metadata(url: str):
    content = fetch_text(url)
    title = parse_meta(content, ["citation_title", "dc.title", "og:title", "twitter:title"])
    if not title:
        m = re.search(r"<title>([\s\S]*?)</title>", content, flags=re.IGNORECASE)
        if m:
            title = html.unescape(re.sub(r"\s+", " ", m.group(1))).strip()
    abstract = parse_meta(content, ["citation_abstract", "description", "dc.description", "og:description", "twitter:description"])
    authors = parse_meta_authors(content)
    return {
        "source": "journal",
        "title": title,
        "abstract": abstract,
        "authors": authors,
        "url": url,
    }


def strip_legacy_prefix(title: str):
    t = title.strip()
    t = re.sub(r"^.*?\(\d{4}\)\.?\s*", "", t)
    t = re.sub(r"^.*?\b\d{4}\b\.?\s*", "", t)
    return t.strip() or title.strip()


def parse_authors_from_legacy_title(title: str):
    m = re.match(r"^(.+?)\(\d{4}\)", title)
    author_zone = m.group(1).strip() if m else title[:220]
    author_zone = author_zone.replace(" & ", ", ").replace(" and ", ", ")
    author_zone = author_zone.replace("...", "")

    found = []
    for last, initials in re.findall(r"([A-Za-z'\-\. ]+?),\s*([A-Z](?:\.[A-Z])*\.?|[A-Z](?:\.\s*[A-Z]\.)+)", author_zone):
        nm = f"{last.strip()}, {initials.strip()}"
        nm = re.sub(r"\s+", " ", nm)
        if nm and nm not in found:
            found.append(nm)

    return found


def normalize_author_name(name: str):
    n = re.sub(r"\s+", " ", name).strip().strip(",")
    if not n:
        return ""
    if "," in n:
        last, rest = [p.strip() for p in n.split(",", 1)]
        parts = [p for p in re.split(r"[\s\-]+", rest.replace(".", " ")) if p]
        initials = " ".join([p[0].upper() + "." for p in parts if p])
        return f"{last}, {initials}".strip()
    parts = n.split(" ")
    if len(parts) == 1:
        return parts[0]
    last = parts[-1]
    initials = " ".join([p[0].upper() + "." for p in parts[:-1] if p])
    return f"{last}, {initials}".strip()


def classify_topics(title: str, abstract: str):
    text = (title + " " + abstract).lower()
    topics = []

    def has(*keys):
        return any(re.search(k, text) for k in keys)

    if has(r"\bpersonalized\b", r"\bindividuali[sz]ed treatment\b", r"\bprecision (health|medicine)\b", r"\bdose finding\b", r"\btreatment rule\b", r"\bheterogeneous treatment\b"):
        topics.append("personalized-medicine")
    if has(r"\breinforcement learning\b", r"\bmarkov decision\b", r"\boff-?policy\b", r"\bdynamic treatment regime\b", r"\bpolicy learning\b", r"\binfinite horizon\b"):
        topics.append("reinforcement-learning")
    if has(r"\brandom forest[s]?\b", r"\bsurvival forest[s]?\b", r"\btree learning\b", r"\bforest models?\b"):
        topics.append("random-forests")
    if has(r"\bsurvival\b", r"\bcensored\b", r"\bmortality\b", r"\btime[- ]to[- ]event\b"):
        topics.append("survival-analysis")
    if has(r"\bdimension reduction\b", r"\binverse regression\b", r"\bsufficient dimension\b", r"\borthodr\b"):
        topics.append("dimension-reduction")
    if has(r"\bnutrition\b", r"\bdiet(ary)?\b", r"\bfood intake\b", r"\bmetabolomic\b", r"\bmetagenomic\b", r"\bmicrobiome\b", r"\bjournal of nutrition\b"):
        topics.append("nutrition-science")
    if has(r"\binfluenza\b", r"\bviral shedding\b", r"\bha head\b", r"\bha stalk\b"):
        topics.append("influenza")
    if has(r"\bsepsis\b", r"\bseptic\b"):
        topics.append("sepsis")

    dedup = []
    for t in topics:
        if t not in dedup and t in ALLOWED_TOPICS:
            dedup.append(t)
    if not dedup:
        dedup = ["others"]
    return dedup


def select_best_metadata(entry):
    fields = entry["fields"]
    title = fields.get("title", "")
    url = fields.get("url", "")
    chosen = None
    notes = []

    url = normalize_url(url)

    if chosen is None and url and not any(x in url for x in ["github.com", "cran.r-project.org", "news.illinois.edu", "igb.illinois.edu", "ncsa.illinois.edu"]):
        try:
            md = fetch_page_metadata(url)
            if md.get("title") or md.get("authors") or md.get("abstract"):
                chosen = md
                notes.append("page")
            time.sleep(0.2)
        except Exception as exc:
            notes.append(f"page-fail:{exc}")

    if chosen is None:
        try:
            query = strip_legacy_prefix(title)
            md = fetch_crossref_metadata(query)
            if md and (md.get("authors") or md.get("title")):
                chosen = md
                notes.append("crossref")
            time.sleep(0.2)
        except Exception as exc:
            notes.append(f"crossref-fail:{exc}")

    if chosen is None:
        chosen = {
            "source": "legacy",
            "title": title,
            "abstract": "",
            "authors": parse_authors_from_legacy_title(title),
            "url": url,
        }
        notes.append("legacy-fallback")

    if not chosen.get("authors"):
        chosen["authors"] = parse_authors_from_legacy_title(title)

    normalized_authors = []
    for a in chosen.get("authors", []):
        na = normalize_author_name(a)
        if na and na not in normalized_authors:
            normalized_authors.append(na)
    chosen["authors_normalized"] = normalized_authors
    chosen["notes"] = notes
    return chosen


def build_report_rows(entries):
    report_rows = []

    for entry in entries:
        key = entry["key"]
        f = entry["fields"]
        kws = parse_keywords(f.get("keywords", ""))
        track = "collaborative" if kws["track"] == "collaborative" else "method-theory"
        venue = normalize_venue(kws["venue"])

        md = select_best_metadata(entry)
        title = md.get("title") or f.get("title", "")
        title = re.sub(r"\s+", " ", title).strip()
        authors = md.get("authors_normalized", [])
        abstract = md.get("abstract", "")
        topics = classify_topics(title, abstract)

        report_rows.append(
            {
                "key": key,
                "source": md.get("source", ""),
                "track": track,
                "venue": venue,
                "topics": topics,
                "authors_count": len(authors),
                "has_abstract": bool(abstract),
                "notes": md.get("notes", []),
            }
        )

    return report_rows


def write_report(rows):
    total = len(rows)
    source_counts = {}
    topic_counts = {}
    no_authors = []

    for r in rows:
        source_counts[r["source"]] = source_counts.get(r["source"], 0) + 1
        for t in r["topics"]:
            topic_counts[t] = topic_counts.get(t, 0) + 1
        if r["authors_count"] == 0:
            no_authors.append(r["key"])

    lines = []
    lines.append("# Publications Enrichment Report")
    lines.append("")
    lines.append(f"Total entries: {total}")
    lines.append("")
    lines.append("## Metadata source counts")
    for k, v in sorted(source_counts.items(), key=lambda x: (-x[1], x[0])):
        lines.append(f"- {k}: {v}")
    lines.append("")
    lines.append("## Topic counts")
    for k, v in sorted(topic_counts.items(), key=lambda x: (-x[1], x[0])):
        lines.append(f"- {k}: {v}")
    lines.append("")
    lines.append("## Entries with no parsed authors")
    if no_authors:
        for k in no_authors:
            lines.append(f"- {k}")
    else:
        lines.append("- none")
    lines.append("")

    REPORT_PATH.write_text("\n".join(lines), encoding="utf-8")


def main():
    text = BIB_PATH.read_text(encoding="utf-8")
    entries = parse_bib_entries(text)
    rows = build_report_rows(entries)
    RAW_PATH.write_text(json.dumps(rows, indent=2), encoding="utf-8")
    write_report(rows)
    print(f"Updated {len(entries)} entries in {RAW_PATH.name} and {REPORT_PATH.name}")


if __name__ == "__main__":
    main()
