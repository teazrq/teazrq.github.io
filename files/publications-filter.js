document.addEventListener("DOMContentLoaded", () => {
  const items = Array.from(document.querySelectorAll("[data-pub-item]"));
  const yearGroups = Array.from(document.querySelectorAll("[data-year-group]"));
  const sections = Array.from(document.querySelectorAll("[data-pub-section]"));
  const filterButtons = Array.from(document.querySelectorAll(".pub-topic-chip[data-topic-filter]"));
  const summary = document.getElementById("pub-static-summary");
  const searchInput = document.getElementById("pub-topic-search");
  const emptyState = document.getElementById("pub-empty-state");
  const clearButton = document.getElementById("pub-clear-filters");
  const allTopicsButton = filterButtons.find((button) => button.dataset.topicFilter === "all");

  if (!items.length) {
    return;
  }

  const activeTopics = new Set();
  const validTopics = new Set(
    filterButtons
      .map((button) => button.dataset.topicFilter)
      .filter((topic) => topic && topic !== "all")
  );

  const topicLabelOverrides = {
    "disease-diagnostics-treatment": "Disease Diagnostics and Treatment",
  };

  const formatTopicLabel = (topic) => (
    topicLabelOverrides[topic]
    || topic
      .split("-")
      .map((part) => part.charAt(0).toUpperCase() + part.slice(1))
      .join(" ")
  );

  const syncButtons = () => {
    for (const button of filterButtons) {
      const topic = button.dataset.topicFilter || "all";
      const isActive = topic === "all"
        ? activeTopics.size === 0
        : activeTopics.has(topic);

      button.classList.toggle("is-active", isActive);
      button.setAttribute("aria-pressed", isActive ? "true" : "false");
    }
  };

  const updateUrl = (method) => {
    const url = new URL(window.location.href);
    url.searchParams.delete("topic");
    url.searchParams.delete("q");

    for (const topic of activeTopics) {
      url.searchParams.append("topic", topic);
    }

    const query = (searchInput?.value || "").trim();
    if (query) {
      url.searchParams.set("q", query);
    }

    const nextUrl = `${url.pathname}${url.search}${url.hash}`;
    window.history[method]({}, "", nextUrl);
  };

  const applyFilters = () => {
    const query = (searchInput?.value || "").trim().toLowerCase();
    let visibleCount = 0;

    for (const item of items) {
      const itemTopics = item.dataset.topics
        ? item.dataset.topics.split("|")
        : [];
      const searchText = item.dataset.search || "";
      const matchesTopic = activeTopics.size === 0
        || Array.from(activeTopics).every((topic) => itemTopics.includes(topic));
      const matchesSearch = !query || searchText.includes(query);
      const visible = matchesTopic && matchesSearch;

      item.hidden = !visible;
      if (visible) {
        visibleCount += 1;
      }
    }

    for (const group of yearGroups) {
      const hasVisibleItems = Array.from(group.querySelectorAll("[data-pub-item]"))
        .some((item) => !item.hidden);
      group.hidden = !hasVisibleItems;
    }

    for (const section of sections) {
      const hasVisibleItems = Array.from(section.querySelectorAll("[data-pub-item]"))
        .some((item) => !item.hidden);
      section.hidden = !hasVisibleItems;
    }

    if (summary) {
      if (visibleCount === items.length && activeTopics.size === 0 && !query) {
        summary.textContent = `Showing ${items.length} publications`;
      } else {
        const topics = Array.from(activeTopics).map(formatTopicLabel);
        let message = `Showing ${visibleCount} of ${items.length} publications`;

        if (topics.length) {
          message += ` for ${topics.join(" and ")}`;
        }
        if (query) {
          message += ` matching “${query}”`;
        }
        summary.textContent = message;
      }
    }

    if (emptyState) {
      emptyState.hidden = visibleCount !== 0;
    }

    syncButtons();
  };

  const readUrlState = () => {
    const params = new URLSearchParams(window.location.search);
    activeTopics.clear();

    for (const topic of params.getAll("topic")) {
      if (validTopics.has(topic)) {
        activeTopics.add(topic);
      }
    }

    if (searchInput) {
      searchInput.value = params.get("q") || "";
    }
  };

  const clearFilters = ({ updateHistory = true, focus = true } = {}) => {
    activeTopics.clear();
    if (searchInput) {
      searchInput.value = "";
    }
    applyFilters();
    if (updateHistory) {
      updateUrl("pushState");
    }
    if (focus) {
      allTopicsButton?.focus();
    }
  };

  for (const button of filterButtons) {
    button.addEventListener("click", () => {
      const topic = button.dataset.topicFilter || "all";

      if (topic === "all") {
        activeTopics.clear();
      } else if (activeTopics.has(topic)) {
        activeTopics.delete(topic);
      } else {
        activeTopics.add(topic);
      }

      applyFilters();
      updateUrl("pushState");
    });
  }

  searchInput?.addEventListener("input", () => {
    applyFilters();
    updateUrl("replaceState");
  });

  clearButton?.addEventListener("click", () => {
    clearFilters();
  });

  window.addEventListener("popstate", () => {
    readUrlState();
    applyFilters();
  });

  readUrlState();
  applyFilters();
});
