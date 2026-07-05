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
