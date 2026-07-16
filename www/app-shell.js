function readCookie(name) {
  var prefix = name + "=";
  var cookies = document.cookie ? document.cookie.split(";") : [];

  for (var i = 0; i < cookies.length; i += 1) {
    var cookie = cookies[i].trim();
    if (cookie.indexOf(prefix) === 0) {
      return decodeURIComponent(cookie.substring(prefix.length));
    }
  }

  return "";
}

function readStoredAuthToken(name) {
  var storageValue = "";

  try {
    storageValue = window.localStorage.getItem(name) || "";
  } catch (error) {
    storageValue = "";
  }

  return storageValue || readCookie(name);
}

function writeCookie(name, value, maxAge) {
  var cookie = name + "=" + encodeURIComponent(value) + "; path=/; SameSite=Lax";

  if (typeof maxAge === "number") {
    cookie += "; max-age=" + maxAge;
  }

  document.cookie = cookie;
}

function writeStoredAuthToken(name, value, maxAge) {
  writeCookie(name, value, maxAge);

  try {
    window.localStorage.setItem(name, value);
  } catch (error) {
    // Ignore storage write issues and keep cookie-based auth.
  }
}

function clearCookie(name) {
  document.cookie = name + "=; path=/; max-age=0; SameSite=Lax";
}

function clearStoredAuthToken(name) {
  clearCookie(name);

  try {
    window.localStorage.removeItem(name);
  } catch (error) {
    // Ignore storage clear issues.
  }
}

window.requestRidsAuthToken = function(inputId, cookieName) {
  if (!window.Shiny || typeof window.Shiny.setInputValue !== "function") {
    return;
  }

  window.Shiny.setInputValue(inputId, readStoredAuthToken(cookieName), {
    priority: "event"
  });
};

Shiny.addCustomMessageHandler("setAppShell", function(isLoggedIn) {
  document.body.classList.toggle("app-shell", !!isLoggedIn);
});

Shiny.addCustomMessageHandler("setAuthCookie", function(payload) {
  writeStoredAuthToken(payload.name, payload.value, payload.maxAge);
});

Shiny.addCustomMessageHandler("clearAuthCookie", function(payload) {
  clearStoredAuthToken(payload.name);
});

Shiny.addCustomMessageHandler("requestAuthCookie", function(payload) {
  window.requestRidsAuthToken(payload.inputId, payload.name);
});

Shiny.addCustomMessageHandler("resetFileInput", function(payload) {
  if (!payload || !payload.id) {
    return;
  }

  var input = document.getElementById(payload.id);

  if (!input) {
    return;
  }

  input.value = "";

  if (window.jQuery) {
    window.jQuery(input).trigger("change");
    window.jQuery(input)
      .closest(".custom-file")
      .find(".custom-file-label")
      .text("Choose Excel File");
  }
});

function visibleFocusableElements(container) {
  if (!container) {
    return [];
  }

  return Array.prototype.filter.call(
    container.querySelectorAll(
      "a[href], button:not([disabled]), input:not([disabled]), " +
      "select:not([disabled]), textarea:not([disabled]), " +
      "[tabindex]:not([tabindex='-1'])"
    ),
    function(element) {
      return element.offsetParent !== null &&
        element.getAttribute("aria-hidden") !== "true";
    }
  );
}

function helpPanelForToggle(toggle) {
  var panelId = toggle && toggle.getAttribute("aria-controls");
  return panelId ? document.getElementById(panelId) : null;
}

function helpToggleForPanel(panel) {
  if (!panel || !panel.id) {
    return null;
  }

  return document.querySelector(
    ".rids-help-toggle[aria-controls='" + panel.id + "']"
  );
}

function focusHelpPanel(panel) {
  var focusable = visibleFocusableElements(panel);
  var target = panel.querySelector(".rids-help-close") || focusable[0] || panel;

  if (target && typeof target.focus === "function") {
    target.focus();
  }
}

function setHelpBackgroundInert(panel, isInert) {
  if (!panel) {
    return;
  }

  if (!isInert) {
    Array.prototype.forEach.call(
      document.querySelectorAll("[data-rids-help-inert='true']"),
      function(element) {
        element.inert = false;
        element.removeAttribute("data-rids-help-inert");
      }
    );
    return;
  }

  var current = panel;
  while (current && current.parentElement) {
    Array.prototype.forEach.call(current.parentElement.children, function(sibling) {
      if (
        sibling !== current &&
        !sibling.inert &&
        !sibling.hasAttribute("data-rids-help-inert")
      ) {
        sibling.inert = true;
        sibling.setAttribute("data-rids-help-inert", "true");
      }
    });
    current = current.parentElement;
  }
}

function openHelpPanel(toggle) {
  var panel = helpPanelForToggle(toggle);

  if (!panel) {
    return;
  }

  panel.style.display = "flex";
  toggle.setAttribute("aria-expanded", "true");
  panel.setAttribute("aria-hidden", "false");
  setHelpBackgroundInert(panel, true);

  window.setTimeout(function() {
    focusHelpPanel(panel);
  }, 75);
}

function closeHelpPanel(panel) {
  if (!panel) {
    return;
  }

  var toggle = helpToggleForPanel(panel);

  panel.style.display = "none";
  panel.setAttribute("aria-hidden", "true");
  setHelpBackgroundInert(panel, false);
  if (toggle) {
    toggle.setAttribute("aria-expanded", "false");
  }

  window.setTimeout(function() {
    if (toggle && typeof toggle.focus === "function") {
      toggle.focus();
    }
  }, 75);
}

function cardTitleForControl(control) {
  var card = control.closest(".card");
  var title = card && card.querySelector(".card-title");
  var text = title ? title.textContent.trim() : "section";
  return text || "section";
}

function syncCardControl(control) {
  var card = control.closest(".card");
  var isCollapsed = card && card.classList.contains("collapsed-card");
  var action = isCollapsed ? "Expand" : "Collapse";
  var label = action + " " + cardTitleForControl(control);

  control.setAttribute("aria-label", label);
  control.setAttribute("title", label);
  control.setAttribute("aria-expanded", isCollapsed ? "false" : "true");
}

function polishAppShellControls(root) {
  var scope = root && root.querySelectorAll ? root : document;

  Array.prototype.forEach.call(
    scope.querySelectorAll("[data-widget='pushmenu']"),
    function(control) {
      control.setAttribute("aria-label", "Toggle navigation");
      control.setAttribute("title", "Toggle navigation");
    }
  );

  Array.prototype.forEach.call(
    scope.querySelectorAll(".card-tools [data-card-widget='collapse']"),
    syncCardControl
  );
}

document.addEventListener("click", function(event) {
  var helpToggle = event.target.closest(".rids-help-toggle");
  var helpClose = event.target.closest(".rids-help-close");
  var cardControl = event.target.closest(
    ".card-tools [data-card-widget='collapse']"
  );

  if (helpToggle) {
    openHelpPanel(helpToggle);
  }

  if (helpClose) {
    closeHelpPanel(helpClose.closest(".rids-help-panel"));
  }

  if (cardControl) {
    window.setTimeout(function() {
      syncCardControl(cardControl);
    }, 0);
  }
});

document.addEventListener("keydown", function(event) {
  var panel = event.target.closest && event.target.closest(".rids-help-panel");

  if (!panel || panel.getAttribute("aria-hidden") === "true") {
    return;
  }

  if (event.key === "Escape") {
    var closeButton = panel.querySelector(".rids-help-close");
    event.preventDefault();
    if (closeButton) {
      closeButton.click();
    }
    return;
  }

  if (event.key !== "Tab") {
    return;
  }

  var focusable = visibleFocusableElements(panel);
  if (focusable.length === 0) {
    event.preventDefault();
    panel.focus();
    return;
  }

  var first = focusable[0];
  var last = focusable[focusable.length - 1];

  if (event.shiftKey && document.activeElement === first) {
    event.preventDefault();
    last.focus();
  } else if (!event.shiftKey && document.activeElement === last) {
    event.preventDefault();
    first.focus();
  }
});

document.addEventListener("DOMContentLoaded", function() {
  polishAppShellControls(document);
});

document.addEventListener("shiny:value", function() {
  window.setTimeout(function() {
    polishAppShellControls(document);
  }, 0);
});
