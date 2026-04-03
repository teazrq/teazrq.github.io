(function () {
  function escapeHtml(value) {
    return String(value)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  function parseCategoryString(raw) {
    var result = {
      track: [],
      venue: [],
      topics: []
    };

    if (!raw) {
      return result;
    }

    var parts = String(raw).split(';');
    parts.forEach(function (part) {
      var chunk = part.trim();
      if (!chunk) {
        return;
      }

      var idx = chunk.indexOf(':');
      if (idx < 0) {
        return;
      }

      var key = chunk.slice(0, idx).trim().toLowerCase();
      var value = chunk.slice(idx + 1).trim();
      if (!value) {
        return;
      }

      var values = value.split(/\||,/).map(function (v) {
        return v.trim().toLowerCase();
      }).filter(Boolean);

      if (key === 'track') {
        result.track = values;
      } else if (key === 'venue') {
        result.venue = values;
      } else if (key === 'topic' || key === 'topics') {
        result.topics = values;
      }
    });

    return result;
  }

  function humanize(value) {
    return value
      .split('-')
      .map(function (word) {
        return word.charAt(0).toUpperCase() + word.slice(1);
      })
      .join(' ');
  }

  function getYear(item) {
    return Number(item.year) || 0;
  }

  function normalizeBibValue(value) {
    return String(value || '')
      .replace(/^\uFEFF/, '')
      .replace(/\s+/g, ' ')
      .trim();
  }

  function extractField(block, fieldName) {
    var pattern = new RegExp(fieldName + '\\s*=\\s*\\{([\\s\\S]*?)\\}\\s*(?:,|$)', 'i');
    var match = block.match(pattern);
    return match ? normalizeBibValue(match[1]) : '';
  }

  function parseBibText(raw) {
    var text = normalizeBibValue(raw);
    var matches = text.match(/@\w+\s*\{[\s\S]*?\n\}/g) || [];

    return matches.map(function (block) {
      var idMatch = block.match(/@\w+\s*\{\s*([^,]+),/);
      var keywords = extractField(block, 'keywords');

      return {
        id: idMatch ? normalizeBibValue(idMatch[1]) : '',
        title: extractField(block, 'title'),
        year: extractField(block, 'year'),
        url: extractField(block, 'url'),
        note: extractField(block, 'note'),
        keywords: keywords,
        categories: parseCategoryString(keywords)
      };
    }).filter(function (item) {
      return item.id && item.title;
    });
  }

  function getLinkLabel(url) {
    if (!url) {
      return '';
    }

    if (url.indexOf('arxiv.org') >= 0) {
      return 'arXiv';
    }
    if (url.indexOf('github.com') >= 0) {
      return 'GitHub';
    }
    if (url.indexOf('cran.r-project.org') >= 0) {
      return 'Package';
    }

    return 'Link';
  }

  function matchesFilters(pub, selected) {
    var groups = ['track', 'venue', 'topics'];

    for (var i = 0; i < groups.length; i += 1) {
      var g = groups[i];
      var chosen = selected[g];
      if (!chosen || chosen.size === 0) {
        continue;
      }

      var values = pub.categories[g] || [];
      var ok = values.some(function (v) {
        return chosen.has(v);
      });

      if (!ok) {
        return false;
      }
    }

    return true;
  }

  function makeBadge(text, kind) {
    var span = document.createElement('span');
    span.className = 'pub-badge pub-badge-' + kind;
    span.textContent = humanize(text);
    return span;
  }

  function buildFilterGroup(name, values, selected) {
    var wrapper = document.createElement('div');
    wrapper.className = 'pub-filter-group';

    var title = document.createElement('div');
    title.className = 'pub-filter-title';
    title.textContent = humanize(name);
    wrapper.appendChild(title);

    values.forEach(function (value) {
      var label = document.createElement('label');
      label.className = 'pub-filter-option';

      var input = document.createElement('input');
      input.type = 'checkbox';
      input.value = value;
      input.checked = false;
      input.addEventListener('change', function () {
        if (input.checked) {
          selected[name].add(value);
        } else {
          selected[name].delete(value);
        }
        window.requestAnimationFrame(window.__renderPublications);
      });

      var text = document.createElement('span');
      text.textContent = humanize(value);

      label.appendChild(input);
      label.appendChild(text);
      wrapper.appendChild(label);
    });

    return wrapper;
  }

  function getEmbeddedBibText() {
    var embedded = document.getElementById('publications-bib-source');
    if (!embedded) {
      return '';
    }

    return (embedded.textContent || embedded.innerText || '').trim();
  }

  async function loadBibText() {
    var isFileProtocol = window.location.protocol === 'file:';

    if (!isFileProtocol) {
      try {
        var res = await fetch('publications.bib', { cache: 'no-store' });
        if (res.ok) {
          var fetched = await res.text();
          if (fetched && fetched.trim()) {
            return fetched;
          }
        }
      } catch (err) {
        // Fall through to embedded source.
      }
    }

    return getEmbeddedBibText();
  }

  async function init() {
    var controls = document.getElementById('pub-controls');
    var summary = document.getElementById('pub-summary');
    var list = document.getElementById('pub-list');

    if (!controls || !summary || !list) {
      return;
    }

    try {
      var bibText = await loadBibText();

      if (!bibText || !bibText.trim()) {
        summary.textContent = 'No publications found in publications.bib yet.';
        return;
      }

      var data = parseBibText(bibText);

      if (data.length === 0) {
        summary.textContent = 'No publications found in publications.bib yet.';
        return;
      }

      var pubs = data.map(function (item) {
        return {
          id: item.id,
          item: item,
          year: getYear(item),
          categories: item.categories
        };
      });

      pubs.sort(function (a, b) {
        return b.year - a.year;
      });

      var categoryValues = {
        track: new Set(),
        venue: new Set(),
        topics: new Set()
      };

      pubs.forEach(function (p) {
        p.categories.track.forEach(function (v) { categoryValues.track.add(v); });
        p.categories.venue.forEach(function (v) { categoryValues.venue.add(v); });
        p.categories.topics.forEach(function (v) { categoryValues.topics.add(v); });
      });

      var selected = {
        track: new Set(),
        venue: new Set(),
        topics: new Set()
      };

      var bar = document.createElement('div');
      bar.className = 'pub-filter-bar';

      var reset = document.createElement('button');
      reset.type = 'button';
      reset.className = 'pub-reset-btn';
      reset.textContent = 'Clear Filters';
      reset.addEventListener('click', function () {
        selected.track.clear();
        selected.venue.clear();
        selected.topics.clear();
        controls.querySelectorAll('input[type="checkbox"]').forEach(function (cb) {
          cb.checked = false;
        });
        window.requestAnimationFrame(window.__renderPublications);
      });

      bar.appendChild(reset);
      controls.appendChild(bar);

      controls.appendChild(buildFilterGroup('track', Array.from(categoryValues.track).sort(), selected));
      controls.appendChild(buildFilterGroup('venue', Array.from(categoryValues.venue).sort(), selected));
      controls.appendChild(buildFilterGroup('topics', Array.from(categoryValues.topics).sort(), selected));

      window.__renderPublications = function () {
        var visible = pubs.filter(function (p) {
          return matchesFilters(p, selected);
        });

        list.innerHTML = '';

        visible.forEach(function (p) {
          var card = document.createElement('div');
          card.className = 'pub-item';

          var badges = document.createElement('div');
          badges.className = 'pub-badges';

          p.categories.track.forEach(function (x) { badges.appendChild(makeBadge(x, 'track')); });
          p.categories.venue.forEach(function (x) { badges.appendChild(makeBadge(x, 'venue')); });
          p.categories.topics.forEach(function (x) { badges.appendChild(makeBadge(x, 'topic')); });

          if (badges.childNodes.length > 0) {
            card.appendChild(badges);
          }

          var ref = document.createElement('div');
          ref.className = 'pub-ref';

          var html = '<div class="pub-citation">' + escapeHtml(p.item.title) + '</div>';
          if (p.item.url) {
            html += '<div class="pub-links"><a href="' +
              escapeHtml(p.item.url) +
              '" target="_blank" rel="noopener noreferrer">' +
              escapeHtml(getLinkLabel(p.item.url)) +
              '</a></div>';
          }
          ref.innerHTML = html;

          card.appendChild(ref);
          list.appendChild(card);
        });

        summary.textContent = 'Showing ' + visible.length + ' of ' + pubs.length + ' publications';
      };

      window.__renderPublications();
    } catch (err) {
      summary.textContent = 'Could not load publications.bib. Please check file format.';
      list.innerHTML = '';
    }
  }

  document.addEventListener('DOMContentLoaded', init);
})();
