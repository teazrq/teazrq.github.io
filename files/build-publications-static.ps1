Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$bibPath = Join-Path (Split-Path -Parent $PSScriptRoot) "publications.bib"
$outputPath = Join-Path $PSScriptRoot "publications-static.html"

$bib = Get-Content -Raw -Path $bibPath
$entryMatches = [regex]::Matches(
  $bib,
  '(?i)@(?!comment\b)\w+\s*\{[\s\S]*?\n\}',
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
    'disease-diagnostics-treatment' { return 'Disease Diagnostics and Treatment' }
    'nutrition-science' { return 'Nutrition Science' }
    'influenza' { return 'Influenza' }
    'sepsis' { return 'Sepsis' }
    'others' { return 'Others' }
    default { return Humanize $slug }
  }
}

function CleanDisplayTitle([string]$title, [string[]]$authors) {
  $t = ($title -replace '\s+', ' ').Trim()

  # Remove source markers that are not part of a publication title.
  $t = $t -replace '(?i)\s+arxiv\b.*$', ''
  $t = $t -replace '(?i)\s+press\s*release\b.*$', ''
  $t = $t -replace '(?i)\s+newsletter(?:\s*editor''?s?\s*commentory)?\b.*$', ''
  $t = $t -replace '(?i)\s+github\s*:\s*\[[^\]]+\].*$', ''
  $t = $t -replace '(?i)\s+r\s*package\s*\[[^\]]+\].*$', ''
  $t = $t -replace '\s*\[[^\]]+\]\s*$', ''

  # Remove a leading author list when a full citation was imported as the title.
  if ($authors -and $authors.Count -ge 2) {
    $firstLast = ($authors[0] -split ',')[0].Trim()
    $secondLast = ($authors[1] -split ',')[0].Trim()
    $lastLast = ($authors[$authors.Count - 1] -split ',')[0].Trim()
    if ($t.StartsWith($firstLast + ',') -and $t.IndexOf($secondLast + ',', [System.StringComparison]::OrdinalIgnoreCase) -ge 0) {
      $pattern = [regex]::Escape($lastLast) + '\s*,\s*.*?\.\s+'
      $match = [regex]::Match($t, $pattern, [System.Text.RegularExpressions.RegexOptions]::IgnoreCase)
      if ($match.Success -and $match.Index -lt 280) {
        $candidate = $t.Substring($match.Index + $match.Length).Trim()
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

function Escape-Html([string]$text) {
  return [System.Net.WebUtility]::HtmlEncode($text)
}

function Split-JournalName([string]$full) {
  if (-not $full) {
    return @('', '')
  }

  $match = [regex]::Match($full, '^(.*\D)\s+(\d+\s*\([\s\S]*)$')
  if ($match.Success) {
    $name = $match.Groups[1].Value.TrimEnd(' ,.').Trim()
    return @($name, $match.Groups[2].Value.Trim())
  }

  $match = [regex]::Match($full, '^(.*?)[,\.]\s+(\d[\s\S]*|Forthcoming[\s\S]*)$')
  if ($match.Success) {
    return @($match.Groups[1].Value.Trim(), $match.Groups[2].Value.Trim())
  }

  return @($full, '')
}

function BuildVenueHtml([string]$journal, [string]$booktitle) {
  if ($journal) {
    $parts = Split-JournalName $journal
    $venueHtml = '<em>' + (Escape-Html $parts[0]) + '</em>'
    if ($parts[1]) {
      $venueHtml += ' ' + (Escape-Html $parts[1])
    }
    return $venueHtml
  }

  if ($booktitle) {
    return 'In <em>' + (Escape-Html $booktitle) + '</em>'
  }

  return ''
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
      $topics = ($matches[1].Trim().TrimEnd('}') -split '\|') |
        ForEach-Object { $_.Trim().TrimEnd('}') } |
        Where-Object { $_ }
    }
  }

  $authors = @()
  if ($author) {
    $authors = ($author -split '\sand\s') |
      ForEach-Object { $_.Trim() } |
      Where-Object { $_ }
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

$entries = @(
  $entries |
    Sort-Object -Property @{ Expression = 'Year'; Descending = $true }, @{ Expression = 'Title'; Descending = $false }
)

if ($entries.Count -eq 0) {
  throw "No publication entries were generated from $bibPath."
}

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
  'disease-diagnostics-treatment',
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
    '" aria-pressed="false" aria-controls="pub-results">' +
    (Escape-Html (TopicLabel $slug)) +
    ' <span>(' + $count + ')</span></button>'
}

$html = New-Object System.Collections.Generic.List[string]
$html.Add('<div class="pub-controls pub-controls-static">')
$html.Add('<fieldset class="pub-filter-group">')
$html.Add('<legend class="pub-filter-title">Filter by topic</legend>')
$html.Add('<div class="pub-topic-filters">')
$html.Add('<button type="button" class="pub-topic-chip is-active" data-topic-filter="all" aria-pressed="true" aria-controls="pub-results">All topics <span>(' + $entries.Count + ')</span></button>')
foreach ($button in $topicButtons) {
  $html.Add($button)
}
$html.Add('</div>')
$html.Add('</fieldset>')
$html.Add('<div class="pub-filter-group pub-search-group" role="search" aria-label="Search publications">')
$html.Add('<label class="pub-filter-title" for="pub-topic-search">Search publications</label>')
$html.Add('<input id="pub-topic-search" class="pub-topic-search" name="q" type="search" autocomplete="off" spellcheck="false" placeholder="Title, author, venue, or keyword&hellip;">')
$html.Add('<p class="pub-scholar-note">For citation counts and a complete profile, visit <a href="https://scholar.google.com/citations?user=uyzMyb8AAAAJ&amp;hl=en">Google Scholar</a>.</p>')
$html.Add('</div>')
$html.Add('</div>')
$html.Add('<p id="pub-static-summary" class="pub-static-summary" role="status" aria-live="polite" aria-atomic="true">Showing ' + $entries.Count + ' publications</p>')
$html.Add('<div id="pub-empty-state" class="pub-empty-state" hidden>')
$html.Add('<p class="pub-empty-title">No matching publications</p>')
$html.Add('<p>Try different keywords or adjust the topic filters.</p>')
$html.Add('<button id="pub-clear-filters" class="pub-clear-btn" type="button">Clear filters</button>')
$html.Add('</div>')
$html.Add('<div id="pub-results" class="pub-results">')

function Add-PublicationItem($entry, $html, [int]$itemNumber) {
  $dataTopics = Escape-Html (($entry.Topics -join '|').ToLowerInvariant())
  $dataSearch = Escape-Html $entry.SearchText
  $displayTitle = CleanDisplayTitle $entry.Title $entry.Authors
  $safeKey = [regex]::Replace($entry.Key, '[^A-Za-z0-9_-]', '-')
  if (-not $safeKey) {
    $safeKey = $entry.Year.ToString() + '-' + $itemNumber
  }
  $titleId = 'pub-title-' + $safeKey

  $html.Add('<article class="pub-item" aria-labelledby="' + (Escape-Html $titleId) + '" data-pub-item data-topics="' + $dataTopics + '" data-search="' + $dataSearch + '">')

  if ($entry.Url) {
    $html.Add('<h4 id="' + (Escape-Html $titleId) + '" class="pub-title"><a href="' + (Escape-Html $entry.Url) + '">' + (Escape-Html $displayTitle) + '</a></h4>')
  } else {
    $html.Add('<h4 id="' + (Escape-Html $titleId) + '" class="pub-title">' + (Escape-Html $displayTitle) + '</h4>')
  }

  if ($entry.Authors -and $entry.Authors.Count -gt 0) {
    $html.Add('<p class="pub-authors">' + (Escape-Html ($entry.Authors -join ', ')) + '</p>')
  }

  $venueHtml = BuildVenueHtml $entry.Journal $entry.BookTitle
  if (-not $venueHtml -and $entry.Venue) {
    $venueHtml = Escape-Html (Humanize $entry.Venue)
  }

  $metaParts = New-Object System.Collections.Generic.List[string]
  if ($venueHtml) {
    $metaParts.Add($venueHtml)
  }
  $metaParts.Add('<span class="pub-year-meta">' + $entry.Year + '</span>')
  $html.Add('<p class="pub-venue-meta">' + ($metaParts -join '<span class="pub-meta-separator" aria-hidden="true"> &middot; </span>') + '</p>')

  $tagLabels = New-Object System.Collections.Generic.List[string]
  if ($entry.Track) {
    $tagLabels.Add((Humanize $entry.Track))
  }
  foreach ($topic in $entry.Topics) {
    $label = TopicLabel $topic
    if (-not $tagLabels.Contains($label)) {
      $tagLabels.Add($label)
    }
  }

  if ($tagLabels.Count -gt 0) {
    $html.Add('<div class="pub-tags" role="list" aria-label="Publication categories">')
    foreach ($label in $tagLabels) {
      $html.Add('<span class="pub-tag" role="listitem">' + (Escape-Html $label) + '</span>')
    }
    $html.Add('</div>')
  }

  if ($entry.Title -eq 'Reinforcement Learning Trees') {
    $html.Add('<p class="pub-links"><a href="https://github.com/teazrq/RLT">RLT GitHub repository</a></p>')
  }

  $html.Add('</article>')
}

$preprints = @($entries | Where-Object { $_.Venue -eq 'preprint' })
$published = @($entries | Where-Object { $_.Venue -ne 'preprint' })
$itemNumber = 0

if ($preprints.Count -gt 0) {
  $html.Add('<section class="pub-section-group" data-pub-section="preprints">')
  $html.Add('<h2 class="pub-section-heading">Preprints</h2>')
  $preprintYears = $preprints | Group-Object -Property Year | Sort-Object { [int]$_.Name } -Descending
  foreach ($yearGroup in $preprintYears) {
    $html.Add('<div class="pub-year-group" data-year-group>')
    $html.Add('<h3 class="pub-year-heading">' + (Escape-Html $yearGroup.Name) + '</h3>')
    $html.Add('<div class="pub-year-items">')
    foreach ($entry in $yearGroup.Group) {
      $itemNumber += 1
      Add-PublicationItem $entry $html $itemNumber
    }
    $html.Add('</div>')
    $html.Add('</div>')
  }
  $html.Add('</section>')
}

if ($published.Count -gt 0) {
  $html.Add('<section class="pub-section-group" data-pub-section="published">')
  $html.Add('<h2 class="pub-section-heading">Published work</h2>')
  $publishedYears = $published | Group-Object -Property Year | Sort-Object { [int]$_.Name } -Descending
  foreach ($yearGroup in $publishedYears) {
    $html.Add('<div class="pub-year-group" data-year-group>')
    $html.Add('<h3 class="pub-year-heading">' + (Escape-Html $yearGroup.Name) + '</h3>')
    $html.Add('<div class="pub-year-items">')
    foreach ($entry in $yearGroup.Group) {
      $itemNumber += 1
      Add-PublicationItem $entry $html $itemNumber
    }
    $html.Add('</div>')
    $html.Add('</div>')
  }
  $html.Add('</section>')
}

$html.Add('</div>')

Set-Content -Path $outputPath -Value $html -Encoding UTF8
