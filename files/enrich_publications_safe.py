import html
import json
import re
import time
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

SKIP_DOMAINS = [
    "github.com",
    "cran.r-project.org",
    "news.illinois.edu",
    "igb.illinois.edu",
    "ncsa.illinois.edu",
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
    blocks = re.findall(r"@misc\s*\{\s*([^,]+),([\s\S]*?)\n\}\s*", text, flags=re.IGNORECASE)
    for key, body in blocks:
        fields = {}
        for name, value in re.findall(r"(\w+)\s*=\s*\{([\s\S]*?)\}\s*,?", body):
            fields[name.lower()] = re.sub(r"\s+", " ", value).strip()
        entries.append({"key": key.strip(), "fields": fields})
    return entries


def parse_keywords(keyword_text: str):
    result = {"track": "method-theory", "venue": "journal"}
    for part in keyword_text.split(";"):
        p = part.strip()
        if p.startswith("track:"):
            result["track"] = p.split(":", 1)[1].strip().lower()
        elif p.startswith("venue:"):
            result["venue"] = p.split(":", 1)[1].strip().lower()
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
        return ""
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


def fetch_page_metadata(url: str):
    content = fetch_text(url)
    title = parse_meta(content, ["citation_title", "dc.title", "og:title", "twitter:title"])
    abstract = parse_meta(content, ["citation_abstract", "description", "dc.description", "og:description", "twitter:description"])
    authors = parse_meta_authors(content)
    source = "journal"
    if "arxiv.org" in url:
        source = "arxiv"
    return {
        "source": source,
        "title": re.sub(r"\s+", " ", title).strip(),
        "abstract": re.sub(r"\s+", " ", abstract).strip(),
        "authors": authors,
    }


def strip_legacy_prefix(title: str):
    t = title.strip()
    t = re.sub(r"^.*?\(\d{4}\)\.?\s*", "", t)
    t = re.sub(r"^.*?\b\d{4}\b\.?\s*", "", t)
    return t.strip() or title.strip()


def clean_title_text(title: str):
    t = re.sub(r"\s+", " ", title or "").strip()

    # Remove leftover markdown link artifacts from legacy imports.
    t = re.sub(r"\[\[link\]\]\([^\)]*\)", "", t, flags=re.IGNORECASE)

    # Remove trailing source markers appended to citation strings.
    t = re.sub(
        r"(?:\s*[\[\(]?(?:arxiv|press\s*release|newsletter(?:\s*editor'?s?\s*commentory)?)\]?\.?\s*)+$",
        "",
        t,
        flags=re.IGNORECASE,
    )

    t = re.sub(r"\s+", " ", t).strip(" .;")
    return t


def parse_authors_from_legacy_title(title: str):
    m = re.match(r"^(.+?)\(\d{4}\)", title)
    zone = m.group(1).strip() if m else title[:220]
    zone = zone.replace(" & ", ", ").replace(" and ", ", ").replace("...", "")

    found = []
    pattern = r"([A-Z][A-Za-z'\-]+(?:\s+[A-Z][A-Za-z'\-]+)*(?:\s+(?:Jr\.?|III|II))?),\s*((?:[A-Z]\.\s*){1,4})"
    for last, initials in re.findall(pattern, zone):
        name = f"{last.strip()}, {initials.strip()}"
        name = re.sub(r"\s+", " ", name)
        if name and name not in found:
            found.append(name)

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

    def has(*patterns):
        return any(re.search(p, text) for p in patterns)

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


def enrich_entry(entry):
    f = entry["fields"]
    original_title = f.get("title", "")
    url = normalize_url(f.get("url", ""))

    md = {"source": "legacy", "title": "", "abstract": "", "authors": []}

    if url and not any(s in url for s in SKIP_DOMAINS):
        try:
            md = fetch_page_metadata(url)
            time.sleep(0.25)
        except Exception:
            md = {"source": "legacy", "title": "", "abstract": "", "authors": []}

    authors_raw = md.get("authors") or parse_authors_from_legacy_title(original_title)
    authors = []
    for a in authors_raw:
        n = normalize_author_name(a)
        if n and n not in authors:
            authors.append(n)

    cleaned_title = clean_title_text(original_title)
    classify_title = md.get("title") or strip_legacy_prefix(cleaned_title)
    topics = classify_topics(classify_title, md.get("abstract", ""))

    return {
        "title": cleaned_title,
        "authors": authors,
        "url": url,
        "source": md.get("source", "legacy"),
        "topics": topics,
        "has_abstract": bool(md.get("abstract")),
    }


def write_outputs(entries):
    report = []

    for entry in entries:
        key = entry["key"]
        f = entry["fields"]
        kws = parse_keywords(f.get("keywords", ""))
        track = "collaborative" if kws["track"] == "collaborative" else "method-theory"
        venue = normalize_venue(kws["venue"])

        enr = enrich_entry(entry)

        report.append(
            {
                "key": key,
                "source": enr["source"],
                "track": track,
                "venue": venue,
                "topics": enr["topics"],
                "authors_count": len(enr["authors"]),
                "has_abstract": enr["has_abstract"],
            }
        )

    RAW_PATH.write_text(json.dumps(report, indent=2), encoding="utf-8")

    source_counts = {}
    topic_counts = {}
    for r in report:
        source_counts[r["source"]] = source_counts.get(r["source"], 0) + 1
        for t in r["topics"]:
            topic_counts[t] = topic_counts.get(t, 0) + 1

    report_lines = [
        "# Publications Enrichment Report",
        "",
        f"Total entries: {len(report)}",
        "",
        "## Metadata source counts",
    ]
    for k, v in sorted(source_counts.items(), key=lambda x: (-x[1], x[0])):
        report_lines.append(f"- {k}: {v}")
    report_lines.append("")
    report_lines.append("## Topic counts")
    for k, v in sorted(topic_counts.items(), key=lambda x: (-x[1], x[0])):
        report_lines.append(f"- {k}: {v}")
    report_lines.append("")

    REPORT_PATH.write_text("\n".join(report_lines), encoding="utf-8")


def main():
    text = BIB_PATH.read_text(encoding="utf-8")
    entries = parse_bib_entries(text)
    write_outputs(entries)
    print(f"Updated {len(entries)} entries in {RAW_PATH.name} and {REPORT_PATH.name}")


if __name__ == "__main__":
    main()
