$bibPath = Join-Path $PSScriptRoot "..\publications.bib"
$outputPath = Join-Path $PSScriptRoot "publications-static.html"

$bib = Get-Content -Raw -Path $bibPath
$entryMatches = [regex]::Matches(
  $bib,
  '@\w+\s*\{[\s\S]*?\n\}',
  [System.Text.RegularExpressions.RegexOptions]::Singleline
)

function Get-Field([string]$block, [string]$name) {
  $match = [regex]::Match(
    $block,
    $name + '\s*=\s*\{([\s\S]*?)\}\s*(?:,|$)',
    [System.Text.RegularExpressions.RegexOptions]::IgnoreCase
  )

  if ($match.Success) {
    return ($match.Groups[1].Value -replace '^\uFEFF', '' -replace '\s+', ' ').Trim()
  }

  return ''
}

function Humanize([string]$text) {
  if (-not $text) {
    return ''
  }

  if ($text -eq 'method-theory') {
    return 'Method & Theory'
  }

  return (($text -split '-') | ForEach-Object {
    if ($_.Length -gt 0) {
      $_.Substring(0, 1).ToUpper() + $_.Substring(1)
    }
  }) -join ' '
}

function TopicLabel([string]$slug) {
  switch ($slug) {
    'personalized-medicine' { return 'Personalized Medicine' }
    'reinforcement-learning' { return 'Reinforcement Learning' }
    'random-forests' { return 'Random Forests' }
    'survival-analysis' { return 'Survival Analysis' }
    'dimension-reduction' { return 'Dimension Reduction' }
    'nutrition-science' { return 'Nutrition Science' }
    'influenza' { return 'Influenza' }
    'sepsis' { return 'Sepsis' }
    'others' { return 'Others' }
    default { return Humanize $slug }
  }
}

function CleanDisplayTitle([string]$title, [string[]]$authors) {
  $t = ($title -replace '\s+', ' ').Trim()

  # Strip source and note markers that are not part of citation text.
  $t = $t -replace '(?i)\s+arxiv\b.*$', ''
  $t = $t -replace '(?i)\s+press\s*release\b.*$', ''
  $t = $t -replace '(?i)\s+newsletter(?:\s*editor''?s?\s*commentory)?\b.*$', ''
  $t = $t -replace '(?i)\s+github\s*:\s*\[[^\]]+\].*$', ''
  $t = $t -replace '(?i)\s+r\s*package\s*\[[^\]]+\].*$', ''
  $t = $t -replace '\s*\[[^\]]+\]\s*$', ''

  # Remove leading author list when title was imported as a full citation.
  if ($authors -and $authors.Count -ge 2) {
    $firstLast = ($authors[0] -split ',')[0].Trim()
    $secondLast = ($authors[1] -split ',')[0].Trim()
    $lastLast = ($authors[$authors.Count - 1] -split ',')[0].Trim()
    if ($t.StartsWith($firstLast + ',') -and $t.IndexOf($secondLast + ',', [System.StringComparison]::OrdinalIgnoreCase) -ge 0) {
      $pat = [regex]::Escape($lastLast) + '\s*,\s*.*?\.\s+'
      $m = [regex]::Match($t, $pat, [System.Text.RegularExpressions.RegexOptions]::IgnoreCase)
      if ($m.Success -and $m.Index -lt 280) {
        $candidate = $t.Substring($m.Index + $m.Length).Trim()
        if ($candidate.Length -gt 20) {
          $t = $candidate
        }
      }
    }
  }

  $firstSentence = [regex]::Match($t, '^(.+?)\.\s+(.+)$')
  if ($firstSentence.Success) {
    $lead = $firstSentence.Groups[1].Value
    $rest = $firstSentence.Groups[2].Value
    if (($lead.Split(',').Count - 1) -ge 3 -and $rest.Length -gt 20) {
      $t = $rest
    }
  }

  return ($t -replace '\s+', ' ').Trim(' ', '.', ';')
}

function Normalize-TitleSentenceCase([string]$title) {
  if (-not $title) {
    return ''
  }

  $tokens = $title -split '\s+'
  if ($tokens.Count -le 1) {
    return $title
  }

  for ($i = 1; $i -lt $tokens.Count; $i++) {
    $token = $tokens[$i]
    $m = [regex]::Match($token, '^([^A-Za-z0-9]*)([A-Za-z0-9][A-Za-z0-9\-/]*)([^A-Za-z0-9]*)$')
    if (-not $m.Success) {
      continue
    }

    $prefix = $m.Groups[1].Value
    $core = $m.Groups[2].Value
    $suffix = $m.Groups[3].Value

    # Keep acronyms/abbreviations and alphanumeric tokens (e.g., FDA, AI/ML, CD64).
    if ($core -cmatch '[0-9]' -or $core -cmatch '^[A-Z]{2,}([\-/][A-Z0-9]{2,})*$') {
      continue
    }

    # Lower title-cased and hyphenated words after the first token.
    if ($core -cmatch '^[A-Z][a-z]+([\-/][A-Za-z]+)*$') {
      $tokens[$i] = $prefix + $core.ToLowerInvariant() + $suffix
    }
  }

  return ($tokens -join ' ')
}

function Escape-Html([string]$text) {
  return [System.Net.WebUtility]::HtmlEncode($text)
}

function Get-LinkLabel([string]$url) {
  if ($url -match 'arxiv\.org') {
    return 'arXiv'
  }

  if ($url -match 'github\.com') {
    return 'GitHub'
  }

  if ($url -match 'cran\.r-project\.org') {
    return 'CRAN'
  }

  return 'Journal'
}

function Split-JournalName([string]$full) {
  if (-not $full) { return @('', '') }

  # Priority: vol(issue) pattern — handles both "Name, 25(1)" and "Name 25(1)"
  $m = [regex]::Match($full, '^(.*\D)\s+(\d+\s*\([\s\S]*)$')
  if ($m.Success) {
    $name = $m.Groups[1].Value.TrimEnd(' ,.').Trim()
    return @($name, $m.Groups[2].Value.Trim())
  }

  # Fallback: comma/period + whitespace + digit or "Forthcoming"
  $m = [regex]::Match($full, '^(.*?)[,\.]\s+(\d[\s\S]*|Forthcoming[\s\S]*)$')
  if ($m.Success) {
    return @($m.Groups[1].Value.Trim(), $m.Groups[2].Value.Trim())
  }

  return @($full, '')
}

function BuildCitationHtml([string]$title, [string]$journal, [string]$booktitle) {
  $venueHtml = ''
  if ($journal) {
    $parts = Split-JournalName $journal
    $jName = $parts[0]
    $jDetails = $parts[1]
    $venueHtml = '<em>' + (Escape-Html $jName) + '</em>'
    if ($jDetails) {
      $venueHtml += ' ' + (Escape-Html $jDetails)
    }
  } elseif ($booktitle) {
    $venueHtml = 'In <em>' + (Escape-Html $booktitle) + '</em>'
  }

  if (-not $title) {
    return $venueHtml
  }

  if ($venueHtml) {
    return (Escape-Html $title) + '. ' + $venueHtml
  }

  return (Escape-Html $title)
}

$entries = foreach ($match in $entryMatches) {
  $block = $match.Value
  $keyMatch = [regex]::Match($block, '^@\w+\s*\{\s*([^,\s]+)', [System.Text.RegularExpressions.RegexOptions]::IgnoreCase)
  $entryKey = ''
  if ($keyMatch.Success) {
    $entryKey = $keyMatch.Groups[1].Value.Trim()
  }
  $title = Get-Field $block 'title'

  if (-not $title) {
    continue
  }

  $year = Get-Field $block 'year'
  $url = Get-Field $block 'url'
  $author = Get-Field $block 'author'
  $journal = Get-Field $block 'journal'
  $booktitle = Get-Field $block 'booktitle'
  $keywords = Get-Field $block 'keywords'
  $track = ''
  $venue = ''
  $topics = @()

  foreach ($part in ($keywords -split ';')) {
    $piece = $part.Trim()

    if ($piece -match '^track:(.+)$') {
      $track = $matches[1].Trim()
    } elseif ($piece -match '^venue:(.+)$') {
      $venue = $matches[1].Trim()
    } elseif ($piece -match '^topics:(.+)$') {
      $topics = ($matches[1].Trim().TrimEnd('}') -split '\|') | ForEach-Object { $_.Trim().TrimEnd('}') } | Where-Object { $_ }
    }
  }

  $authors = @()
  if ($author) {
    $authors = ($author -split '\sand\s') | ForEach-Object { $_.Trim() } | Where-Object { $_ }
  }

  [pscustomobject]@{
    Key = $entryKey
    Year = [int]$year
    Title = $title
    Journal = $journal
    BookTitle = $booktitle
    Authors = $authors
    Url = $url
    Track = $track
    Venue = $venue
    Topics = $topics
    SearchText = (($title, $journal, $booktitle, ($authors -join ' '), (Humanize $track), (Humanize $venue), (($topics | ForEach-Object { TopicLabel $_ }) -join ' ')) -join ' ').ToLowerInvariant()
  }
}

$entries = $entries | Sort-Object -Property @{ Expression = 'Year'; Descending = $true }, @{ Expression = 'Title'; Descending = $false }

$topicCounts = @{}
foreach ($entry in $entries) {
  foreach ($topic in $entry.Topics) {
    if (-not $topicCounts.ContainsKey($topic)) {
      $topicCounts[$topic] = 0
    }
    $topicCounts[$topic] += 1
  }
}

$topicOrder = @(
  'personalized-medicine',
  'reinforcement-learning',
  'random-forests',
  'survival-analysis',
  'dimension-reduction',
  'nutrition-science',
  'influenza',
  'sepsis',
  'others'
)

$topicButtons = foreach ($slug in $topicOrder) {
  $count = 0
  if ($topicCounts.ContainsKey($slug)) {
    $count = $topicCounts[$slug]
  }
  '<button type="button" class="pub-topic-chip" data-topic-filter="' +
    (Escape-Html $slug) +
    '">' +
    (Escape-Html (TopicLabel $slug)) +
    ' <span>(' + $count + ')</span></button>'
}

$html = New-Object System.Collections.Generic.List[string]
$html.Add('<div class="pub-controls pub-controls-static">')
$html.Add('<div class="pub-filter-group">')
$html.Add('<div class="pub-filter-title">Filter by topic</div>')
$html.Add('<div class="pub-topic-filters">')
$html.Add('<button type="button" class="pub-topic-chip is-active" data-topic-filter="all">All Topics <span>(' + $entries.Count + ')</span></button>')
foreach ($button in $topicButtons) {
  $html.Add($button)
}
$html.Add('</div>')
$html.Add('</div>')
$html.Add('<div class="pub-filter-group pub-search-group">')
$html.Add('<label class="pub-filter-title" for="pub-topic-search">Search within current results</label>')
$html.Add('<input id="pub-topic-search" class="pub-topic-search" type="search" placeholder="Try random forest, sepsis, inverse regression...">')
$html.Add('<p class="pub-scholar-note">A full list of publications is available on <a href="https://scholar.google.com/citations?user=uyzMyb8AAAAJ&amp;hl=en" target="_blank" rel="noopener noreferrer">Google Scholar</a>.</p>')
$html.Add('</div>')
$html.Add('</div>')

function Add-PublicationItem($entry, $html) {
  $dataTopics = Escape-Html (($entry.Topics -join '|').ToLowerInvariant())
  $dataSearch = Escape-Html $entry.SearchText
  $html.Add('<div class="pub-item" data-pub-item data-topics="' + $dataTopics + '" data-search="' + $dataSearch + '">')
  $html.Add('<div class="pub-badges">')

  if ($entry.Track) {
    $html.Add('<span class="pub-badge pub-badge-track">' + (Escape-Html (Humanize $entry.Track)) + '</span>')
  }

  if ($entry.Venue) {
    $html.Add('<span class="pub-badge pub-badge-venue">' + (Escape-Html (Humanize $entry.Venue)) + '</span>')
  }

  foreach ($topic in $entry.Topics) {
    $html.Add('<button type="button" class="pub-badge pub-badge-topic pub-badge-button" data-topic-filter="' +
      (Escape-Html $topic) +
      '">' +
      (Escape-Html (TopicLabel $topic)) +
      '</button>')
  }

  $links = New-Object System.Collections.Generic.List[object]

  if ($entry.Url) {
    $links.Add([pscustomobject]@{
      Url = $entry.Url
      Label = Get-LinkLabel $entry.Url
    })
  }

  if ($entry.Title -eq 'Reinforcement Learning Trees') {
    $links.Add([pscustomobject]@{
      Url = 'https://github.com/teazrq/RLT'
      Label = 'GitHub Ver. 4+'
    })
  }

  foreach ($link in $links) {
    if ($link.Label -eq 'Journal') {
      continue
    }

    $safeUrl = Escape-Html $link.Url
    $safeLabel = Escape-Html $link.Label
    $html.Add('<a class="pub-badge pub-badge-link" href="' + $safeUrl + '" target="_blank" rel="noopener noreferrer">' + $safeLabel + '</a>')
  }

  $html.Add('</div>')
  if ($entry.Authors -and $entry.Authors.Count -gt 0) {
    $html.Add('<div class="pub-authors">' + (Escape-Html (($entry.Authors -join ', '))) + '</div>')
  }
  $displayTitle = Normalize-TitleSentenceCase (CleanDisplayTitle $entry.Title $entry.Authors)
  $citationHtml = BuildCitationHtml $displayTitle $entry.Journal $entry.BookTitle
  $html.Add('<div class="pub-citation">' + $citationHtml + ' (' + $entry.Year + ')</div>')

  $html.Add('</div>')
}

$preprints = @($entries | Where-Object { $_.Venue -eq 'preprint' })
$published = @($entries | Where-Object { $_.Venue -ne 'preprint' })

if ($preprints.Count -gt 0) {
  $html.Add('<section class="pub-section-group" data-pub-section="preprint">')
  $html.Add('<h2 class="pub-section-heading">Pre-print</h2>')
  foreach ($entry in $preprints) {
    Add-PublicationItem $entry $html
  }
  $html.Add('</section>')
}

if ($published.Count -gt 0) {
  $html.Add('<section class="pub-section-group" data-pub-section="published">')
  $html.Add('<h2 class="pub-section-heading">Publications</h2>')
  foreach ($entry in $published) {
    Add-PublicationItem $entry $html
  }
  $html.Add('</section>')
}

Set-Content -Path $outputPath -Value $html -Encoding UTF8