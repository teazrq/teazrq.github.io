document.addEventListener("DOMContentLoaded", () => {
  const items = Array.from(document.querySelectorAll("[data-pub-item]"));
  const groups = Array.from(document.querySelectorAll("[data-pub-section], [data-year-group]"));
  const filterButtons = Array.from(document.querySelectorAll("[data-topic-filter]"));
  const summary = document.getElementById("pub-static-summary");
  const searchInput = document.getElementById("pub-topic-search");

  if (!items.length) {
    return;
  }

  const activeTopics = new Set();

  const formatTopicLabel = (topic) => (
    topic
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

  const applyFilters = () => {
    const query = (searchInput?.value || "").trim().toLowerCase();
    let visibleCount = 0;

    for (const item of items) {
      const topicText = item.dataset.topics || "";
      const searchText = item.dataset.search || "";
      const itemTopics = topicText ? topicText.split("|") : [];
      const matchesTopic = activeTopics.size === 0
        || Array.from(activeTopics).every((topic) => itemTopics.includes(topic));
      const matchesSearch = !query || searchText.includes(query);
      const visible = matchesTopic && matchesSearch;

      item.hidden = !visible;
      if (visible) {
        visibleCount += 1;
      }
    }

    for (const group of groups) {
      const hasVisibleItems = Array.from(group.querySelectorAll("[data-pub-item]")).some((item) => !item.hidden);
      group.hidden = !hasVisibleItems;
    }

    const topicLabel = activeTopics.size === 0
      ? "all topics"
      : Array.from(activeTopics).map(formatTopicLabel).join(", ");

    if (summary) {
      if (query) {
        summary.textContent = `Showing ${visibleCount} publications for ${topicLabel} matching "${query}"`;
      } else {
        summary.textContent = `Showing ${visibleCount} publications for ${topicLabel}`;
      }
    }

    syncButtons();
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
    });
  }

  searchInput?.addEventListener("input", applyFilters);
  applyFilters();
});