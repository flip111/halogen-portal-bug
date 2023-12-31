// modules are defined as an array
// [ module function, map of requires ]
//
// map of requires is short require name -> numeric require
//
// anything defined in a previous bundle is accessed via the
// orig method which is the require for previous bundles

(function (modules, entry, mainEntry, parcelRequireName, globalName) {
  /* eslint-disable no-undef */
  var globalObject =
    typeof globalThis !== 'undefined'
      ? globalThis
      : typeof self !== 'undefined'
      ? self
      : typeof window !== 'undefined'
      ? window
      : typeof global !== 'undefined'
      ? global
      : {};
  /* eslint-enable no-undef */

  // Save the require from previous bundle to this closure if any
  var previousRequire =
    typeof globalObject[parcelRequireName] === 'function' &&
    globalObject[parcelRequireName];

  var cache = previousRequire.cache || {};
  // Do not use `require` to prevent Webpack from trying to bundle this call
  var nodeRequire =
    typeof module !== 'undefined' &&
    typeof module.require === 'function' &&
    module.require.bind(module);

  function newRequire(name, jumped) {
    if (!cache[name]) {
      if (!modules[name]) {
        // if we cannot find the module within our internal map or
        // cache jump to the current global require ie. the last bundle
        // that was added to the page.
        var currentRequire =
          typeof globalObject[parcelRequireName] === 'function' &&
          globalObject[parcelRequireName];
        if (!jumped && currentRequire) {
          return currentRequire(name, true);
        }

        // If there are other bundles on this page the require from the
        // previous one is saved to 'previousRequire'. Repeat this as
        // many times as there are bundles until the module is found or
        // we exhaust the require chain.
        if (previousRequire) {
          return previousRequire(name, true);
        }

        // Try the node require function if it exists.
        if (nodeRequire && typeof name === 'string') {
          return nodeRequire(name);
        }

        var err = new Error("Cannot find module '" + name + "'");
        err.code = 'MODULE_NOT_FOUND';
        throw err;
      }

      localRequire.resolve = resolve;
      localRequire.cache = {};

      var module = (cache[name] = new newRequire.Module(name));

      modules[name][0].call(
        module.exports,
        localRequire,
        module,
        module.exports,
        this
      );
    }

    return cache[name].exports;

    function localRequire(x) {
      var res = localRequire.resolve(x);
      return res === false ? {} : newRequire(res);
    }

    function resolve(x) {
      var id = modules[name][1][x];
      return id != null ? id : x;
    }
  }

  function Module(moduleName) {
    this.id = moduleName;
    this.bundle = newRequire;
    this.exports = {};
  }

  newRequire.isParcelRequire = true;
  newRequire.Module = Module;
  newRequire.modules = modules;
  newRequire.cache = cache;
  newRequire.parent = previousRequire;
  newRequire.register = function (id, exports) {
    modules[id] = [
      function (require, module) {
        module.exports = exports;
      },
      {},
    ];
  };

  Object.defineProperty(newRequire, 'root', {
    get: function () {
      return globalObject[parcelRequireName];
    },
  });

  globalObject[parcelRequireName] = newRequire;

  for (var i = 0; i < entry.length; i++) {
    newRequire(entry[i]);
  }

  if (mainEntry) {
    // Expose entry point to Node, AMD or browser globals
    // Based on https://github.com/ForbesLindesay/umd/blob/master/template.js
    var mainExports = newRequire(mainEntry);

    // CommonJS
    if (typeof exports === 'object' && typeof module !== 'undefined') {
      module.exports = mainExports;

      // RequireJS
    } else if (typeof define === 'function' && define.amd) {
      define(function () {
        return mainExports;
      });

      // <script>
    } else if (globalName) {
      this[globalName] = mainExports;
    }
  }
})({"20dt5":[function(require,module,exports) {
var global = arguments[3];
var HMR_HOST = null;
var HMR_PORT = null;
var HMR_SECURE = false;
var HMR_ENV_HASH = "d6ea1d42532a7575";
module.bundle.HMR_BUNDLE_ID = "dc4214eef019e09c";
"use strict";
/* global HMR_HOST, HMR_PORT, HMR_ENV_HASH, HMR_SECURE, chrome, browser, __parcel__import__, __parcel__importScripts__, ServiceWorkerGlobalScope */ /*::
import type {
  HMRAsset,
  HMRMessage,
} from '@parcel/reporter-dev-server/src/HMRServer.js';
interface ParcelRequire {
  (string): mixed;
  cache: {|[string]: ParcelModule|};
  hotData: {|[string]: mixed|};
  Module: any;
  parent: ?ParcelRequire;
  isParcelRequire: true;
  modules: {|[string]: [Function, {|[string]: string|}]|};
  HMR_BUNDLE_ID: string;
  root: ParcelRequire;
}
interface ParcelModule {
  hot: {|
    data: mixed,
    accept(cb: (Function) => void): void,
    dispose(cb: (mixed) => void): void,
    // accept(deps: Array<string> | string, cb: (Function) => void): void,
    // decline(): void,
    _acceptCallbacks: Array<(Function) => void>,
    _disposeCallbacks: Array<(mixed) => void>,
  |};
}
interface ExtensionContext {
  runtime: {|
    reload(): void,
    getURL(url: string): string;
    getManifest(): {manifest_version: number, ...};
  |};
}
declare var module: {bundle: ParcelRequire, ...};
declare var HMR_HOST: string;
declare var HMR_PORT: string;
declare var HMR_ENV_HASH: string;
declare var HMR_SECURE: boolean;
declare var chrome: ExtensionContext;
declare var browser: ExtensionContext;
declare var __parcel__import__: (string) => Promise<void>;
declare var __parcel__importScripts__: (string) => Promise<void>;
declare var globalThis: typeof self;
declare var ServiceWorkerGlobalScope: Object;
*/ var OVERLAY_ID = "__parcel__error__overlay__";
var OldModule = module.bundle.Module;
function Module(moduleName) {
    OldModule.call(this, moduleName);
    this.hot = {
        data: module.bundle.hotData[moduleName],
        _acceptCallbacks: [],
        _disposeCallbacks: [],
        accept: function(fn) {
            this._acceptCallbacks.push(fn || function() {});
        },
        dispose: function(fn) {
            this._disposeCallbacks.push(fn);
        }
    };
    module.bundle.hotData[moduleName] = undefined;
}
module.bundle.Module = Module;
module.bundle.hotData = {};
var checkedAssets /*: {|[string]: boolean|} */ , assetsToDispose /*: Array<[ParcelRequire, string]> */ , assetsToAccept /*: Array<[ParcelRequire, string]> */ ;
function getHostname() {
    return HMR_HOST || (location.protocol.indexOf("http") === 0 ? location.hostname : "localhost");
}
function getPort() {
    return HMR_PORT || location.port;
}
// eslint-disable-next-line no-redeclare
var parent = module.bundle.parent;
if ((!parent || !parent.isParcelRequire) && typeof WebSocket !== "undefined") {
    var hostname = getHostname();
    var port = getPort();
    var protocol = HMR_SECURE || location.protocol == "https:" && !/localhost|127.0.0.1|0.0.0.0/.test(hostname) ? "wss" : "ws";
    var ws = new WebSocket(protocol + "://" + hostname + (port ? ":" + port : "") + "/");
    // Web extension context
    var extCtx = typeof chrome === "undefined" ? typeof browser === "undefined" ? null : browser : chrome;
    // Safari doesn't support sourceURL in error stacks.
    // eval may also be disabled via CSP, so do a quick check.
    var supportsSourceURL = false;
    try {
        (0, eval)('throw new Error("test"); //# sourceURL=test.js');
    } catch (err) {
        supportsSourceURL = err.stack.includes("test.js");
    }
    // $FlowFixMe
    ws.onmessage = async function(event /*: {data: string, ...} */ ) {
        checkedAssets = {} /*: {|[string]: boolean|} */ ;
        assetsToAccept = [];
        assetsToDispose = [];
        var data /*: HMRMessage */  = JSON.parse(event.data);
        if (data.type === "update") {
            // Remove error overlay if there is one
            if (typeof document !== "undefined") removeErrorOverlay();
            let assets = data.assets.filter((asset)=>asset.envHash === HMR_ENV_HASH);
            // Handle HMR Update
            let handled = assets.every((asset)=>{
                return asset.type === "css" || asset.type === "js" && hmrAcceptCheck(module.bundle.root, asset.id, asset.depsByBundle);
            });
            if (handled) {
                console.clear();
                // Dispatch custom event so other runtimes (e.g React Refresh) are aware.
                if (typeof window !== "undefined" && typeof CustomEvent !== "undefined") window.dispatchEvent(new CustomEvent("parcelhmraccept"));
                await hmrApplyUpdates(assets);
                // Dispose all old assets.
                let processedAssets = {} /*: {|[string]: boolean|} */ ;
                for(let i = 0; i < assetsToDispose.length; i++){
                    let id = assetsToDispose[i][1];
                    if (!processedAssets[id]) {
                        hmrDispose(assetsToDispose[i][0], id);
                        processedAssets[id] = true;
                    }
                }
                // Run accept callbacks. This will also re-execute other disposed assets in topological order.
                processedAssets = {};
                for(let i = 0; i < assetsToAccept.length; i++){
                    let id = assetsToAccept[i][1];
                    if (!processedAssets[id]) {
                        hmrAccept(assetsToAccept[i][0], id);
                        processedAssets[id] = true;
                    }
                }
            } else fullReload();
        }
        if (data.type === "error") {
            // Log parcel errors to console
            for (let ansiDiagnostic of data.diagnostics.ansi){
                let stack = ansiDiagnostic.codeframe ? ansiDiagnostic.codeframe : ansiDiagnostic.stack;
                console.error("\uD83D\uDEA8 [parcel]: " + ansiDiagnostic.message + "\n" + stack + "\n\n" + ansiDiagnostic.hints.join("\n"));
            }
            if (typeof document !== "undefined") {
                // Render the fancy html overlay
                removeErrorOverlay();
                var overlay = createErrorOverlay(data.diagnostics.html);
                // $FlowFixMe
                document.body.appendChild(overlay);
            }
        }
    };
    ws.onerror = function(e) {
        console.error(e.message);
    };
    ws.onclose = function() {
        console.warn("[parcel] \uD83D\uDEA8 Connection to the HMR server was lost");
    };
}
function removeErrorOverlay() {
    var overlay = document.getElementById(OVERLAY_ID);
    if (overlay) {
        overlay.remove();
        console.log("[parcel] ✨ Error resolved");
    }
}
function createErrorOverlay(diagnostics) {
    var overlay = document.createElement("div");
    overlay.id = OVERLAY_ID;
    let errorHTML = '<div style="background: black; opacity: 0.85; font-size: 16px; color: white; position: fixed; height: 100%; width: 100%; top: 0px; left: 0px; padding: 30px; font-family: Menlo, Consolas, monospace; z-index: 9999;">';
    for (let diagnostic of diagnostics){
        let stack = diagnostic.frames.length ? diagnostic.frames.reduce((p, frame)=>{
            return `${p}
<a href="/__parcel_launch_editor?file=${encodeURIComponent(frame.location)}" style="text-decoration: underline; color: #888" onclick="fetch(this.href); return false">${frame.location}</a>
${frame.code}`;
        }, "") : diagnostic.stack;
        errorHTML += `
      <div>
        <div style="font-size: 18px; font-weight: bold; margin-top: 20px;">
          🚨 ${diagnostic.message}
        </div>
        <pre>${stack}</pre>
        <div>
          ${diagnostic.hints.map((hint)=>"<div>\uD83D\uDCA1 " + hint + "</div>").join("")}
        </div>
        ${diagnostic.documentation ? `<div>📝 <a style="color: violet" href="${diagnostic.documentation}" target="_blank">Learn more</a></div>` : ""}
      </div>
    `;
    }
    errorHTML += "</div>";
    overlay.innerHTML = errorHTML;
    return overlay;
}
function fullReload() {
    if ("reload" in location) location.reload();
    else if (extCtx && extCtx.runtime && extCtx.runtime.reload) extCtx.runtime.reload();
}
function getParents(bundle, id) /*: Array<[ParcelRequire, string]> */ {
    var modules = bundle.modules;
    if (!modules) return [];
    var parents = [];
    var k, d, dep;
    for(k in modules)for(d in modules[k][1]){
        dep = modules[k][1][d];
        if (dep === id || Array.isArray(dep) && dep[dep.length - 1] === id) parents.push([
            bundle,
            k
        ]);
    }
    if (bundle.parent) parents = parents.concat(getParents(bundle.parent, id));
    return parents;
}
function updateLink(link) {
    var href = link.getAttribute("href");
    if (!href) return;
    var newLink = link.cloneNode();
    newLink.onload = function() {
        if (link.parentNode !== null) // $FlowFixMe
        link.parentNode.removeChild(link);
    };
    newLink.setAttribute("href", // $FlowFixMe
    href.split("?")[0] + "?" + Date.now());
    // $FlowFixMe
    link.parentNode.insertBefore(newLink, link.nextSibling);
}
var cssTimeout = null;
function reloadCSS() {
    if (cssTimeout) return;
    cssTimeout = setTimeout(function() {
        var links = document.querySelectorAll('link[rel="stylesheet"]');
        for(var i = 0; i < links.length; i++){
            // $FlowFixMe[incompatible-type]
            var href /*: string */  = links[i].getAttribute("href");
            var hostname = getHostname();
            var servedFromHMRServer = hostname === "localhost" ? new RegExp("^(https?:\\/\\/(0.0.0.0|127.0.0.1)|localhost):" + getPort()).test(href) : href.indexOf(hostname + ":" + getPort());
            var absolute = /^https?:\/\//i.test(href) && href.indexOf(location.origin) !== 0 && !servedFromHMRServer;
            if (!absolute) updateLink(links[i]);
        }
        cssTimeout = null;
    }, 50);
}
function hmrDownload(asset) {
    if (asset.type === "js") {
        if (typeof document !== "undefined") {
            let script = document.createElement("script");
            script.src = asset.url + "?t=" + Date.now();
            if (asset.outputFormat === "esmodule") script.type = "module";
            return new Promise((resolve, reject)=>{
                var _document$head;
                script.onload = ()=>resolve(script);
                script.onerror = reject;
                (_document$head = document.head) === null || _document$head === void 0 || _document$head.appendChild(script);
            });
        } else if (typeof importScripts === "function") {
            // Worker scripts
            if (asset.outputFormat === "esmodule") return import(asset.url + "?t=" + Date.now());
            else return new Promise((resolve, reject)=>{
                try {
                    importScripts(asset.url + "?t=" + Date.now());
                    resolve();
                } catch (err) {
                    reject(err);
                }
            });
        }
    }
}
async function hmrApplyUpdates(assets) {
    global.parcelHotUpdate = Object.create(null);
    let scriptsToRemove;
    try {
        // If sourceURL comments aren't supported in eval, we need to load
        // the update from the dev server over HTTP so that stack traces
        // are correct in errors/logs. This is much slower than eval, so
        // we only do it if needed (currently just Safari).
        // https://bugs.webkit.org/show_bug.cgi?id=137297
        // This path is also taken if a CSP disallows eval.
        if (!supportsSourceURL) {
            let promises = assets.map((asset)=>{
                var _hmrDownload;
                return (_hmrDownload = hmrDownload(asset)) === null || _hmrDownload === void 0 ? void 0 : _hmrDownload.catch((err)=>{
                    // Web extension bugfix for Chromium
                    // https://bugs.chromium.org/p/chromium/issues/detail?id=1255412#c12
                    if (extCtx && extCtx.runtime && extCtx.runtime.getManifest().manifest_version == 3) {
                        if (typeof ServiceWorkerGlobalScope != "undefined" && global instanceof ServiceWorkerGlobalScope) {
                            extCtx.runtime.reload();
                            return;
                        }
                        asset.url = extCtx.runtime.getURL("/__parcel_hmr_proxy__?url=" + encodeURIComponent(asset.url + "?t=" + Date.now()));
                        return hmrDownload(asset);
                    }
                    throw err;
                });
            });
            scriptsToRemove = await Promise.all(promises);
        }
        assets.forEach(function(asset) {
            hmrApply(module.bundle.root, asset);
        });
    } finally{
        delete global.parcelHotUpdate;
        if (scriptsToRemove) scriptsToRemove.forEach((script)=>{
            if (script) {
                var _document$head2;
                (_document$head2 = document.head) === null || _document$head2 === void 0 || _document$head2.removeChild(script);
            }
        });
    }
}
function hmrApply(bundle /*: ParcelRequire */ , asset /*:  HMRAsset */ ) {
    var modules = bundle.modules;
    if (!modules) return;
    if (asset.type === "css") reloadCSS();
    else if (asset.type === "js") {
        let deps = asset.depsByBundle[bundle.HMR_BUNDLE_ID];
        if (deps) {
            if (modules[asset.id]) {
                // Remove dependencies that are removed and will become orphaned.
                // This is necessary so that if the asset is added back again, the cache is gone, and we prevent a full page reload.
                let oldDeps = modules[asset.id][1];
                for(let dep in oldDeps)if (!deps[dep] || deps[dep] !== oldDeps[dep]) {
                    let id = oldDeps[dep];
                    let parents = getParents(module.bundle.root, id);
                    if (parents.length === 1) hmrDelete(module.bundle.root, id);
                }
            }
            if (supportsSourceURL) // Global eval. We would use `new Function` here but browser
            // support for source maps is better with eval.
            (0, eval)(asset.output);
            // $FlowFixMe
            let fn = global.parcelHotUpdate[asset.id];
            modules[asset.id] = [
                fn,
                deps
            ];
        } else if (bundle.parent) hmrApply(bundle.parent, asset);
    }
}
function hmrDelete(bundle, id) {
    let modules = bundle.modules;
    if (!modules) return;
    if (modules[id]) {
        // Collect dependencies that will become orphaned when this module is deleted.
        let deps = modules[id][1];
        let orphans = [];
        for(let dep in deps){
            let parents = getParents(module.bundle.root, deps[dep]);
            if (parents.length === 1) orphans.push(deps[dep]);
        }
        // Delete the module. This must be done before deleting dependencies in case of circular dependencies.
        delete modules[id];
        delete bundle.cache[id];
        // Now delete the orphans.
        orphans.forEach((id)=>{
            hmrDelete(module.bundle.root, id);
        });
    } else if (bundle.parent) hmrDelete(bundle.parent, id);
}
function hmrAcceptCheck(bundle /*: ParcelRequire */ , id /*: string */ , depsByBundle /*: ?{ [string]: { [string]: string } }*/ ) {
    if (hmrAcceptCheckOne(bundle, id, depsByBundle)) return true;
    // Traverse parents breadth first. All possible ancestries must accept the HMR update, or we'll reload.
    let parents = getParents(module.bundle.root, id);
    let accepted = false;
    while(parents.length > 0){
        let v = parents.shift();
        let a = hmrAcceptCheckOne(v[0], v[1], null);
        if (a) // If this parent accepts, stop traversing upward, but still consider siblings.
        accepted = true;
        else {
            // Otherwise, queue the parents in the next level upward.
            let p = getParents(module.bundle.root, v[1]);
            if (p.length === 0) {
                // If there are no parents, then we've reached an entry without accepting. Reload.
                accepted = false;
                break;
            }
            parents.push(...p);
        }
    }
    return accepted;
}
function hmrAcceptCheckOne(bundle /*: ParcelRequire */ , id /*: string */ , depsByBundle /*: ?{ [string]: { [string]: string } }*/ ) {
    var modules = bundle.modules;
    if (!modules) return;
    if (depsByBundle && !depsByBundle[bundle.HMR_BUNDLE_ID]) {
        // If we reached the root bundle without finding where the asset should go,
        // there's nothing to do. Mark as "accepted" so we don't reload the page.
        if (!bundle.parent) return true;
        return hmrAcceptCheck(bundle.parent, id, depsByBundle);
    }
    if (checkedAssets[id]) return true;
    checkedAssets[id] = true;
    var cached = bundle.cache[id];
    assetsToDispose.push([
        bundle,
        id
    ]);
    if (!cached || cached.hot && cached.hot._acceptCallbacks.length) {
        assetsToAccept.push([
            bundle,
            id
        ]);
        return true;
    }
}
function hmrDispose(bundle /*: ParcelRequire */ , id /*: string */ ) {
    var cached = bundle.cache[id];
    bundle.hotData[id] = {};
    if (cached && cached.hot) cached.hot.data = bundle.hotData[id];
    if (cached && cached.hot && cached.hot._disposeCallbacks.length) cached.hot._disposeCallbacks.forEach(function(cb) {
        cb(bundle.hotData[id]);
    });
    delete bundle.cache[id];
}
function hmrAccept(bundle /*: ParcelRequire */ , id /*: string */ ) {
    // Execute the module.
    bundle(id);
    // Run the accept callbacks in the new version of the module.
    var cached = bundle.cache[id];
    if (cached && cached.hot && cached.hot._acceptCallbacks.length) cached.hot._acceptCallbacks.forEach(function(cb) {
        var assetsToAlsoAccept = cb(function() {
            return getParents(module.bundle.root, id);
        });
        if (assetsToAlsoAccept && assetsToAccept.length) {
            assetsToAlsoAccept.forEach(function(a) {
                hmrDispose(a[0], a[1]);
            });
            // $FlowFixMe[method-unbinding]
            assetsToAccept.push.apply(assetsToAccept, assetsToAlsoAccept);
        }
    });
}

},{}],"ck57E":[function(require,module,exports) {
(()=>{
    // output/Control.Semigroupoid/index.js
    var semigroupoidFn = {
        compose: function(f) {
            return function(g) {
                return function(x) {
                    return f(g(x));
                };
            };
        }
    };
    var compose = function(dict) {
        return dict.compose;
    };
    // output/Control.Category/index.js
    var identity = function(dict) {
        return dict.identity;
    };
    var categoryFn = {
        identity: function(x) {
            return x;
        },
        Semigroupoid0: function() {
            return semigroupoidFn;
        }
    };
    // output/Data.Boolean/index.js
    var otherwise = true;
    // output/Data.Function/index.js
    var flip = function(f) {
        return function(b2) {
            return function(a2) {
                return f(a2)(b2);
            };
        };
    };
    var $$const = function(a2) {
        return function(v) {
            return a2;
        };
    };
    // output/Data.Functor/foreign.js
    var arrayMap = function(f) {
        return function(arr) {
            var l = arr.length;
            var result = new Array(l);
            for(var i2 = 0; i2 < l; i2++)result[i2] = f(arr[i2]);
            return result;
        };
    };
    // output/Data.Unit/foreign.js
    var unit = void 0;
    // output/Type.Proxy/index.js
    var $$Proxy = /* @__PURE__ */ function() {
        function $$Proxy2() {}
        $$Proxy2.value = new $$Proxy2();
        return $$Proxy2;
    }();
    // output/Data.Functor/index.js
    var map = function(dict) {
        return dict.map;
    };
    var $$void = function(dictFunctor) {
        return map(dictFunctor)($$const(unit));
    };
    var voidLeft = function(dictFunctor) {
        var map110 = map(dictFunctor);
        return function(f) {
            return function(x) {
                return map110($$const(x))(f);
            };
        };
    };
    var voidRight = function(dictFunctor) {
        var map110 = map(dictFunctor);
        return function(x) {
            return map110($$const(x));
        };
    };
    var functorFn = {
        map: /* @__PURE__ */ compose(semigroupoidFn)
    };
    var functorArray = {
        map: arrayMap
    };
    // output/Control.Apply/index.js
    var identity2 = /* @__PURE__ */ identity(categoryFn);
    var apply = function(dict) {
        return dict.apply;
    };
    var applySecond = function(dictApply) {
        var apply1 = apply(dictApply);
        var map26 = map(dictApply.Functor0());
        return function(a2) {
            return function(b2) {
                return apply1(map26($$const(identity2))(a2))(b2);
            };
        };
    };
    // output/Control.Applicative/index.js
    var pure = function(dict) {
        return dict.pure;
    };
    var unless = function(dictApplicative) {
        var pure13 = pure(dictApplicative);
        return function(v) {
            return function(v1) {
                if (!v) return v1;
                if (v) return pure13(unit);
                throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [
                    v.constructor.name,
                    v1.constructor.name
                ]);
            };
        };
    };
    var when = function(dictApplicative) {
        var pure13 = pure(dictApplicative);
        return function(v) {
            return function(v1) {
                if (v) return v1;
                if (!v) return pure13(unit);
                throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [
                    v.constructor.name,
                    v1.constructor.name
                ]);
            };
        };
    };
    var liftA1 = function(dictApplicative) {
        var apply2 = apply(dictApplicative.Apply0());
        var pure13 = pure(dictApplicative);
        return function(f) {
            return function(a2) {
                return apply2(pure13(f))(a2);
            };
        };
    };
    // output/Control.Bind/index.js
    var discard = function(dict) {
        return dict.discard;
    };
    var bind = function(dict) {
        return dict.bind;
    };
    var bindFlipped = function(dictBind) {
        return flip(bind(dictBind));
    };
    var composeKleisliFlipped = function(dictBind) {
        var bindFlipped12 = bindFlipped(dictBind);
        return function(f) {
            return function(g) {
                return function(a2) {
                    return bindFlipped12(f)(g(a2));
                };
            };
        };
    };
    var discardUnit = {
        discard: function(dictBind) {
            return bind(dictBind);
        }
    };
    // output/Effect.Aff/foreign.js
    var Aff = function() {
        var EMPTY = {};
        var PURE = "Pure";
        var THROW = "Throw";
        var CATCH = "Catch";
        var SYNC = "Sync";
        var ASYNC = "Async";
        var BIND = "Bind";
        var BRACKET = "Bracket";
        var FORK = "Fork";
        var SEQ = "Sequential";
        var MAP = "Map";
        var APPLY = "Apply";
        var ALT = "Alt";
        var CONS = "Cons";
        var RESUME = "Resume";
        var RELEASE = "Release";
        var FINALIZER = "Finalizer";
        var FINALIZED = "Finalized";
        var FORKED = "Forked";
        var FIBER = "Fiber";
        var THUNK = "Thunk";
        function Aff2(tag, _1, _2, _3) {
            this.tag = tag;
            this._1 = _1;
            this._2 = _2;
            this._3 = _3;
        }
        function AffCtr(tag) {
            var fn = function(_1, _2, _3) {
                return new Aff2(tag, _1, _2, _3);
            };
            fn.tag = tag;
            return fn;
        }
        function nonCanceler2(error4) {
            return new Aff2(PURE, void 0);
        }
        function runEff(eff) {
            try {
                eff();
            } catch (error4) {
                setTimeout(function() {
                    throw error4;
                }, 0);
            }
        }
        function runSync(left, right, eff) {
            try {
                return right(eff());
            } catch (error4) {
                return left(error4);
            }
        }
        function runAsync(left, eff, k) {
            try {
                return eff(k)();
            } catch (error4) {
                k(left(error4))();
                return nonCanceler2;
            }
        }
        var Scheduler = function() {
            var limit = 1024;
            var size4 = 0;
            var ix = 0;
            var queue = new Array(limit);
            var draining = false;
            function drain() {
                var thunk;
                draining = true;
                while(size4 !== 0){
                    size4--;
                    thunk = queue[ix];
                    queue[ix] = void 0;
                    ix = (ix + 1) % limit;
                    thunk();
                }
                draining = false;
            }
            return {
                isDraining: function() {
                    return draining;
                },
                enqueue: function(cb) {
                    var i2, tmp;
                    if (size4 === limit) {
                        tmp = draining;
                        drain();
                        draining = tmp;
                    }
                    queue[(ix + size4) % limit] = cb;
                    size4++;
                    if (!draining) drain();
                }
            };
        }();
        function Supervisor(util) {
            var fibers = {};
            var fiberId = 0;
            var count = 0;
            return {
                register: function(fiber) {
                    var fid = fiberId++;
                    fiber.onComplete({
                        rethrow: true,
                        handler: function(result) {
                            return function() {
                                count--;
                                delete fibers[fid];
                            };
                        }
                    })();
                    fibers[fid] = fiber;
                    count++;
                },
                isEmpty: function() {
                    return count === 0;
                },
                killAll: function(killError, cb) {
                    return function() {
                        if (count === 0) return cb();
                        var killCount = 0;
                        var kills = {};
                        function kill4(fid) {
                            kills[fid] = fibers[fid].kill(killError, function(result) {
                                return function() {
                                    delete kills[fid];
                                    killCount--;
                                    if (util.isLeft(result) && util.fromLeft(result)) setTimeout(function() {
                                        throw util.fromLeft(result);
                                    }, 0);
                                    if (killCount === 0) cb();
                                };
                            })();
                        }
                        for(var k in fibers)if (fibers.hasOwnProperty(k)) {
                            killCount++;
                            kill4(k);
                        }
                        fibers = {};
                        fiberId = 0;
                        count = 0;
                        return function(error4) {
                            return new Aff2(SYNC, function() {
                                for(var k2 in kills)if (kills.hasOwnProperty(k2)) kills[k2]();
                            });
                        };
                    };
                }
            };
        }
        var SUSPENDED = 0;
        var CONTINUE = 1;
        var STEP_BIND = 2;
        var STEP_RESULT = 3;
        var PENDING = 4;
        var RETURN = 5;
        var COMPLETED = 6;
        function Fiber(util, supervisor, aff) {
            var runTick = 0;
            var status2 = SUSPENDED;
            var step3 = aff;
            var fail2 = null;
            var interrupt = null;
            var bhead = null;
            var btail = null;
            var attempts = null;
            var bracketCount = 0;
            var joinId = 0;
            var joins = null;
            var rethrow = true;
            function run3(localRunTick) {
                var tmp, result, attempt;
                while(true){
                    tmp = null;
                    result = null;
                    attempt = null;
                    switch(status2){
                        case STEP_BIND:
                            status2 = CONTINUE;
                            try {
                                step3 = bhead(step3);
                                if (btail === null) bhead = null;
                                else {
                                    bhead = btail._1;
                                    btail = btail._2;
                                }
                            } catch (e) {
                                status2 = RETURN;
                                fail2 = util.left(e);
                                step3 = null;
                            }
                            break;
                        case STEP_RESULT:
                            if (util.isLeft(step3)) {
                                status2 = RETURN;
                                fail2 = step3;
                                step3 = null;
                            } else if (bhead === null) status2 = RETURN;
                            else {
                                status2 = STEP_BIND;
                                step3 = util.fromRight(step3);
                            }
                            break;
                        case CONTINUE:
                            switch(step3.tag){
                                case BIND:
                                    if (bhead) btail = new Aff2(CONS, bhead, btail);
                                    bhead = step3._2;
                                    status2 = CONTINUE;
                                    step3 = step3._1;
                                    break;
                                case PURE:
                                    if (bhead === null) {
                                        status2 = RETURN;
                                        step3 = util.right(step3._1);
                                    } else {
                                        status2 = STEP_BIND;
                                        step3 = step3._1;
                                    }
                                    break;
                                case SYNC:
                                    status2 = STEP_RESULT;
                                    step3 = runSync(util.left, util.right, step3._1);
                                    break;
                                case ASYNC:
                                    status2 = PENDING;
                                    step3 = runAsync(util.left, step3._1, function(result2) {
                                        return function() {
                                            if (runTick !== localRunTick) return;
                                            runTick++;
                                            Scheduler.enqueue(function() {
                                                if (runTick !== localRunTick + 1) return;
                                                status2 = STEP_RESULT;
                                                step3 = result2;
                                                run3(runTick);
                                            });
                                        };
                                    });
                                    return;
                                case THROW:
                                    status2 = RETURN;
                                    fail2 = util.left(step3._1);
                                    step3 = null;
                                    break;
                                case CATCH:
                                    if (bhead === null) attempts = new Aff2(CONS, step3, attempts, interrupt);
                                    else attempts = new Aff2(CONS, step3, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                                    bhead = null;
                                    btail = null;
                                    status2 = CONTINUE;
                                    step3 = step3._1;
                                    break;
                                case BRACKET:
                                    bracketCount++;
                                    if (bhead === null) attempts = new Aff2(CONS, step3, attempts, interrupt);
                                    else attempts = new Aff2(CONS, step3, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                                    bhead = null;
                                    btail = null;
                                    status2 = CONTINUE;
                                    step3 = step3._1;
                                    break;
                                case FORK:
                                    status2 = STEP_RESULT;
                                    tmp = Fiber(util, supervisor, step3._2);
                                    if (supervisor) supervisor.register(tmp);
                                    if (step3._1) tmp.run();
                                    step3 = util.right(tmp);
                                    break;
                                case SEQ:
                                    status2 = CONTINUE;
                                    step3 = sequential3(util, supervisor, step3._1);
                                    break;
                            }
                            break;
                        case RETURN:
                            bhead = null;
                            btail = null;
                            if (attempts === null) {
                                status2 = COMPLETED;
                                step3 = interrupt || fail2 || step3;
                            } else {
                                tmp = attempts._3;
                                attempt = attempts._1;
                                attempts = attempts._2;
                                switch(attempt.tag){
                                    case CATCH:
                                        if (interrupt && interrupt !== tmp && bracketCount === 0) status2 = RETURN;
                                        else if (fail2) {
                                            status2 = CONTINUE;
                                            step3 = attempt._2(util.fromLeft(fail2));
                                            fail2 = null;
                                        }
                                        break;
                                    case RESUME:
                                        if (interrupt && interrupt !== tmp && bracketCount === 0 || fail2) status2 = RETURN;
                                        else {
                                            bhead = attempt._1;
                                            btail = attempt._2;
                                            status2 = STEP_BIND;
                                            step3 = util.fromRight(step3);
                                        }
                                        break;
                                    case BRACKET:
                                        bracketCount--;
                                        if (fail2 === null) {
                                            result = util.fromRight(step3);
                                            attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                                            if (interrupt === tmp || bracketCount > 0) {
                                                status2 = CONTINUE;
                                                step3 = attempt._3(result);
                                            }
                                        }
                                        break;
                                    case RELEASE:
                                        attempts = new Aff2(CONS, new Aff2(FINALIZED, step3, fail2), attempts, interrupt);
                                        status2 = CONTINUE;
                                        if (interrupt && interrupt !== tmp && bracketCount === 0) step3 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                                        else if (fail2) step3 = attempt._1.failed(util.fromLeft(fail2))(attempt._2);
                                        else step3 = attempt._1.completed(util.fromRight(step3))(attempt._2);
                                        fail2 = null;
                                        bracketCount++;
                                        break;
                                    case FINALIZER:
                                        bracketCount++;
                                        attempts = new Aff2(CONS, new Aff2(FINALIZED, step3, fail2), attempts, interrupt);
                                        status2 = CONTINUE;
                                        step3 = attempt._1;
                                        break;
                                    case FINALIZED:
                                        bracketCount--;
                                        status2 = RETURN;
                                        step3 = attempt._1;
                                        fail2 = attempt._2;
                                        break;
                                }
                            }
                            break;
                        case COMPLETED:
                            for(var k in joins)if (joins.hasOwnProperty(k)) {
                                rethrow = rethrow && joins[k].rethrow;
                                runEff(joins[k].handler(step3));
                            }
                            joins = null;
                            if (interrupt && fail2) setTimeout(function() {
                                throw util.fromLeft(fail2);
                            }, 0);
                            else if (util.isLeft(step3) && rethrow) setTimeout(function() {
                                if (rethrow) throw util.fromLeft(step3);
                            }, 0);
                            return;
                        case SUSPENDED:
                            status2 = CONTINUE;
                            break;
                        case PENDING:
                            return;
                    }
                }
            }
            function onComplete(join4) {
                return function() {
                    if (status2 === COMPLETED) {
                        rethrow = rethrow && join4.rethrow;
                        join4.handler(step3)();
                        return function() {};
                    }
                    var jid = joinId++;
                    joins = joins || {};
                    joins[jid] = join4;
                    return function() {
                        if (joins !== null) delete joins[jid];
                    };
                };
            }
            function kill4(error4, cb) {
                return function() {
                    if (status2 === COMPLETED) {
                        cb(util.right(void 0))();
                        return function() {};
                    }
                    var canceler = onComplete({
                        rethrow: false,
                        handler: function() {
                            return cb(util.right(void 0));
                        }
                    })();
                    switch(status2){
                        case SUSPENDED:
                            interrupt = util.left(error4);
                            status2 = COMPLETED;
                            step3 = interrupt;
                            run3(runTick);
                            break;
                        case PENDING:
                            if (interrupt === null) interrupt = util.left(error4);
                            if (bracketCount === 0) {
                                if (status2 === PENDING) attempts = new Aff2(CONS, new Aff2(FINALIZER, step3(error4)), attempts, interrupt);
                                status2 = RETURN;
                                step3 = null;
                                fail2 = null;
                                run3(++runTick);
                            }
                            break;
                        default:
                            if (interrupt === null) interrupt = util.left(error4);
                            if (bracketCount === 0) {
                                status2 = RETURN;
                                step3 = null;
                                fail2 = null;
                            }
                    }
                    return canceler;
                };
            }
            function join3(cb) {
                return function() {
                    var canceler = onComplete({
                        rethrow: false,
                        handler: cb
                    })();
                    if (status2 === SUSPENDED) run3(runTick);
                    return canceler;
                };
            }
            return {
                kill: kill4,
                join: join3,
                onComplete,
                isSuspended: function() {
                    return status2 === SUSPENDED;
                },
                run: function() {
                    if (status2 === SUSPENDED) {
                        if (!Scheduler.isDraining()) Scheduler.enqueue(function() {
                            run3(runTick);
                        });
                        else run3(runTick);
                    }
                }
            };
        }
        function runPar(util, supervisor, par, cb) {
            var fiberId = 0;
            var fibers = {};
            var killId = 0;
            var kills = {};
            var early = new Error("[ParAff] Early exit");
            var interrupt = null;
            var root = EMPTY;
            function kill4(error4, par2, cb2) {
                var step3 = par2;
                var head2 = null;
                var tail = null;
                var count = 0;
                var kills2 = {};
                var tmp, kid;
                loop: while(true){
                    tmp = null;
                    switch(step3.tag){
                        case FORKED:
                            if (step3._3 === EMPTY) {
                                tmp = fibers[step3._1];
                                kills2[count++] = tmp.kill(error4, function(result) {
                                    return function() {
                                        count--;
                                        if (count === 0) cb2(result)();
                                    };
                                });
                            }
                            if (head2 === null) break loop;
                            step3 = head2._2;
                            if (tail === null) head2 = null;
                            else {
                                head2 = tail._1;
                                tail = tail._2;
                            }
                            break;
                        case MAP:
                            step3 = step3._2;
                            break;
                        case APPLY:
                        case ALT:
                            if (head2) tail = new Aff2(CONS, head2, tail);
                            head2 = step3;
                            step3 = step3._1;
                            break;
                    }
                }
                if (count === 0) cb2(util.right(void 0))();
                else {
                    kid = 0;
                    tmp = count;
                    for(; kid < tmp; kid++)kills2[kid] = kills2[kid]();
                }
                return kills2;
            }
            function join3(result, head2, tail) {
                var fail2, step3, lhs, rhs, tmp, kid;
                if (util.isLeft(result)) {
                    fail2 = result;
                    step3 = null;
                } else {
                    step3 = result;
                    fail2 = null;
                }
                loop: while(true){
                    lhs = null;
                    rhs = null;
                    tmp = null;
                    kid = null;
                    if (interrupt !== null) return;
                    if (head2 === null) {
                        cb(fail2 || step3)();
                        return;
                    }
                    if (head2._3 !== EMPTY) return;
                    switch(head2.tag){
                        case MAP:
                            if (fail2 === null) {
                                head2._3 = util.right(head2._1(util.fromRight(step3)));
                                step3 = head2._3;
                            } else head2._3 = fail2;
                            break;
                        case APPLY:
                            lhs = head2._1._3;
                            rhs = head2._2._3;
                            if (fail2) {
                                head2._3 = fail2;
                                tmp = true;
                                kid = killId++;
                                kills[kid] = kill4(early, fail2 === lhs ? head2._2 : head2._1, function() {
                                    return function() {
                                        delete kills[kid];
                                        if (tmp) tmp = false;
                                        else if (tail === null) join3(fail2, null, null);
                                        else join3(fail2, tail._1, tail._2);
                                    };
                                });
                                if (tmp) {
                                    tmp = false;
                                    return;
                                }
                            } else if (lhs === EMPTY || rhs === EMPTY) return;
                            else {
                                step3 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                                head2._3 = step3;
                            }
                            break;
                        case ALT:
                            lhs = head2._1._3;
                            rhs = head2._2._3;
                            if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) return;
                            if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                                fail2 = step3 === lhs ? rhs : lhs;
                                step3 = null;
                                head2._3 = fail2;
                            } else {
                                head2._3 = step3;
                                tmp = true;
                                kid = killId++;
                                kills[kid] = kill4(early, step3 === lhs ? head2._2 : head2._1, function() {
                                    return function() {
                                        delete kills[kid];
                                        if (tmp) tmp = false;
                                        else if (tail === null) join3(step3, null, null);
                                        else join3(step3, tail._1, tail._2);
                                    };
                                });
                                if (tmp) {
                                    tmp = false;
                                    return;
                                }
                            }
                            break;
                    }
                    if (tail === null) head2 = null;
                    else {
                        head2 = tail._1;
                        tail = tail._2;
                    }
                }
            }
            function resolve(fiber) {
                return function(result) {
                    return function() {
                        delete fibers[fiber._1];
                        fiber._3 = result;
                        join3(result, fiber._2._1, fiber._2._2);
                    };
                };
            }
            function run3() {
                var status2 = CONTINUE;
                var step3 = par;
                var head2 = null;
                var tail = null;
                var tmp, fid;
                loop: while(true){
                    tmp = null;
                    fid = null;
                    switch(status2){
                        case CONTINUE:
                            switch(step3.tag){
                                case MAP:
                                    if (head2) tail = new Aff2(CONS, head2, tail);
                                    head2 = new Aff2(MAP, step3._1, EMPTY, EMPTY);
                                    step3 = step3._2;
                                    break;
                                case APPLY:
                                    if (head2) tail = new Aff2(CONS, head2, tail);
                                    head2 = new Aff2(APPLY, EMPTY, step3._2, EMPTY);
                                    step3 = step3._1;
                                    break;
                                case ALT:
                                    if (head2) tail = new Aff2(CONS, head2, tail);
                                    head2 = new Aff2(ALT, EMPTY, step3._2, EMPTY);
                                    step3 = step3._1;
                                    break;
                                default:
                                    fid = fiberId++;
                                    status2 = RETURN;
                                    tmp = step3;
                                    step3 = new Aff2(FORKED, fid, new Aff2(CONS, head2, tail), EMPTY);
                                    tmp = Fiber(util, supervisor, tmp);
                                    tmp.onComplete({
                                        rethrow: false,
                                        handler: resolve(step3)
                                    })();
                                    fibers[fid] = tmp;
                                    if (supervisor) supervisor.register(tmp);
                            }
                            break;
                        case RETURN:
                            if (head2 === null) break loop;
                            if (head2._1 === EMPTY) {
                                head2._1 = step3;
                                status2 = CONTINUE;
                                step3 = head2._2;
                                head2._2 = EMPTY;
                            } else {
                                head2._2 = step3;
                                step3 = head2;
                                if (tail === null) head2 = null;
                                else {
                                    head2 = tail._1;
                                    tail = tail._2;
                                }
                            }
                    }
                }
                root = step3;
                for(fid = 0; fid < fiberId; fid++)fibers[fid].run();
            }
            function cancel(error4, cb2) {
                interrupt = util.left(error4);
                var innerKills;
                for(var kid in kills)if (kills.hasOwnProperty(kid)) {
                    innerKills = kills[kid];
                    for(kid in innerKills)if (innerKills.hasOwnProperty(kid)) innerKills[kid]();
                }
                kills = null;
                var newKills = kill4(error4, root, cb2);
                return function(killError) {
                    return new Aff2(ASYNC, function(killCb) {
                        return function() {
                            for(var kid2 in newKills)if (newKills.hasOwnProperty(kid2)) newKills[kid2]();
                            return nonCanceler2;
                        };
                    });
                };
            }
            run3();
            return function(killError) {
                return new Aff2(ASYNC, function(killCb) {
                    return function() {
                        return cancel(killError, killCb);
                    };
                });
            };
        }
        function sequential3(util, supervisor, par) {
            return new Aff2(ASYNC, function(cb) {
                return function() {
                    return runPar(util, supervisor, par, cb);
                };
            });
        }
        Aff2.EMPTY = EMPTY;
        Aff2.Pure = AffCtr(PURE);
        Aff2.Throw = AffCtr(THROW);
        Aff2.Catch = AffCtr(CATCH);
        Aff2.Sync = AffCtr(SYNC);
        Aff2.Async = AffCtr(ASYNC);
        Aff2.Bind = AffCtr(BIND);
        Aff2.Bracket = AffCtr(BRACKET);
        Aff2.Fork = AffCtr(FORK);
        Aff2.Seq = AffCtr(SEQ);
        Aff2.ParMap = AffCtr(MAP);
        Aff2.ParApply = AffCtr(APPLY);
        Aff2.ParAlt = AffCtr(ALT);
        Aff2.Fiber = Fiber;
        Aff2.Supervisor = Supervisor;
        Aff2.Scheduler = Scheduler;
        Aff2.nonCanceler = nonCanceler2;
        return Aff2;
    }();
    var _pure = Aff.Pure;
    var _throwError = Aff.Throw;
    function _catchError(aff) {
        return function(k) {
            return Aff.Catch(aff, k);
        };
    }
    function _map(f) {
        return function(aff) {
            if (aff.tag === Aff.Pure.tag) return Aff.Pure(f(aff._1));
            else return Aff.Bind(aff, function(value12) {
                return Aff.Pure(f(value12));
            });
        };
    }
    function _bind(aff) {
        return function(k) {
            return Aff.Bind(aff, k);
        };
    }
    function _fork(immediate) {
        return function(aff) {
            return Aff.Fork(immediate, aff);
        };
    }
    var _liftEffect = Aff.Sync;
    function _parAffMap(f) {
        return function(aff) {
            return Aff.ParMap(f, aff);
        };
    }
    function _parAffApply(aff1) {
        return function(aff2) {
            return Aff.ParApply(aff1, aff2);
        };
    }
    var makeAff = Aff.Async;
    function generalBracket(acquire) {
        return function(options2) {
            return function(k) {
                return Aff.Bracket(acquire, options2, k);
            };
        };
    }
    function _makeFiber(util, aff) {
        return function() {
            return Aff.Fiber(util, null, aff);
        };
    }
    var _delay = function() {
        function setDelay(n, k) {
            if (n === 0 && typeof setImmediate !== "undefined") return setImmediate(k);
            else return setTimeout(k, n);
        }
        function clearDelay(n, t) {
            if (n === 0 && typeof clearImmediate !== "undefined") return clearImmediate(t);
            else return clearTimeout(t);
        }
        return function(right, ms) {
            return Aff.Async(function(cb) {
                return function() {
                    var timer = setDelay(ms, cb(right()));
                    return function() {
                        return Aff.Sync(function() {
                            return right(clearDelay(ms, timer));
                        });
                    };
                };
            });
        };
    }();
    var _sequential = Aff.Seq;
    // output/Control.Monad/index.js
    var unlessM = function(dictMonad) {
        var bind9 = bind(dictMonad.Bind1());
        var unless2 = unless(dictMonad.Applicative0());
        return function(mb) {
            return function(m) {
                return bind9(mb)(function(b2) {
                    return unless2(b2)(m);
                });
            };
        };
    };
    var ap = function(dictMonad) {
        var bind9 = bind(dictMonad.Bind1());
        var pure13 = pure(dictMonad.Applicative0());
        return function(f) {
            return function(a2) {
                return bind9(f)(function(f$prime) {
                    return bind9(a2)(function(a$prime) {
                        return pure13(f$prime(a$prime));
                    });
                });
            };
        };
    };
    // output/Data.Semigroup/foreign.js
    var concatArray = function(xs) {
        return function(ys) {
            if (xs.length === 0) return ys;
            if (ys.length === 0) return xs;
            return xs.concat(ys);
        };
    };
    // output/Data.Symbol/index.js
    var reflectSymbol = function(dict) {
        return dict.reflectSymbol;
    };
    // output/Data.Semigroup/index.js
    var semigroupArray = {
        append: concatArray
    };
    var append = function(dict) {
        return dict.append;
    };
    // output/Data.Bounded/foreign.js
    var topChar = String.fromCharCode(65535);
    var bottomChar = String.fromCharCode(0);
    var topNumber = Number.POSITIVE_INFINITY;
    var bottomNumber = Number.NEGATIVE_INFINITY;
    // output/Data.Ord/foreign.js
    var unsafeCompareImpl = function(lt) {
        return function(eq2) {
            return function(gt) {
                return function(x) {
                    return function(y) {
                        return x < y ? lt : x === y ? eq2 : gt;
                    };
                };
            };
        };
    };
    var ordIntImpl = unsafeCompareImpl;
    var ordStringImpl = unsafeCompareImpl;
    // output/Data.Eq/foreign.js
    var refEq = function(r1) {
        return function(r2) {
            return r1 === r2;
        };
    };
    var eqIntImpl = refEq;
    var eqStringImpl = refEq;
    // output/Data.Eq/index.js
    var eqUnit = {
        eq: function(v) {
            return function(v1) {
                return true;
            };
        }
    };
    var eqString = {
        eq: eqStringImpl
    };
    var eqInt = {
        eq: eqIntImpl
    };
    var eq = function(dict) {
        return dict.eq;
    };
    // output/Data.Ordering/index.js
    var LT = /* @__PURE__ */ function() {
        function LT2() {}
        LT2.value = new LT2();
        return LT2;
    }();
    var GT = /* @__PURE__ */ function() {
        function GT2() {}
        GT2.value = new GT2();
        return GT2;
    }();
    var EQ = /* @__PURE__ */ function() {
        function EQ2() {}
        EQ2.value = new EQ2();
        return EQ2;
    }();
    // output/Data.Ord/index.js
    var ordUnit = {
        compare: function(v) {
            return function(v1) {
                return EQ.value;
            };
        },
        Eq0: function() {
            return eqUnit;
        }
    };
    var ordString = /* @__PURE__ */ function() {
        return {
            compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
            Eq0: function() {
                return eqString;
            }
        };
    }();
    var ordInt = /* @__PURE__ */ function() {
        return {
            compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
            Eq0: function() {
                return eqInt;
            }
        };
    }();
    var compare = function(dict) {
        return dict.compare;
    };
    // output/Data.Show/foreign.js
    var showIntImpl = function(n) {
        return n.toString();
    };
    // output/Data.Show/index.js
    var showInt = {
        show: showIntImpl
    };
    var show = function(dict) {
        return dict.show;
    };
    // output/Data.Maybe/index.js
    var identity3 = /* @__PURE__ */ identity(categoryFn);
    var Nothing = /* @__PURE__ */ function() {
        function Nothing2() {}
        Nothing2.value = new Nothing2();
        return Nothing2;
    }();
    var Just = /* @__PURE__ */ function() {
        function Just2(value0) {
            this.value0 = value0;
        }
        Just2.create = function(value0) {
            return new Just2(value0);
        };
        return Just2;
    }();
    var maybe$prime = function(v) {
        return function(v1) {
            return function(v2) {
                if (v2 instanceof Nothing) return v(unit);
                if (v2 instanceof Just) return v1(v2.value0);
                throw new Error("Failed pattern match at Data.Maybe (line 250, column 1 - line 250, column 62): " + [
                    v.constructor.name,
                    v1.constructor.name,
                    v2.constructor.name
                ]);
            };
        };
    };
    var maybe = function(v) {
        return function(v1) {
            return function(v2) {
                if (v2 instanceof Nothing) return v;
                if (v2 instanceof Just) return v1(v2.value0);
                throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [
                    v.constructor.name,
                    v1.constructor.name,
                    v2.constructor.name
                ]);
            };
        };
    };
    var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
    var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
    var functorMaybe = {
        map: function(v) {
            return function(v1) {
                if (v1 instanceof Just) return new Just(v(v1.value0));
                return Nothing.value;
            };
        }
    };
    var map2 = /* @__PURE__ */ map(functorMaybe);
    var fromMaybe = function(a2) {
        return maybe(a2)(identity3);
    };
    var fromJust = function() {
        return function(v) {
            if (v instanceof Just) return v.value0;
            throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [
                v.constructor.name
            ]);
        };
    };
    var applyMaybe = {
        apply: function(v) {
            return function(v1) {
                if (v instanceof Just) return map2(v.value0)(v1);
                if (v instanceof Nothing) return Nothing.value;
                throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [
                    v.constructor.name,
                    v1.constructor.name
                ]);
            };
        },
        Functor0: function() {
            return functorMaybe;
        }
    };
    var bindMaybe = {
        bind: function(v) {
            return function(v1) {
                if (v instanceof Just) return v1(v.value0);
                if (v instanceof Nothing) return Nothing.value;
                throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [
                    v.constructor.name,
                    v1.constructor.name
                ]);
            };
        },
        Apply0: function() {
            return applyMaybe;
        }
    };
    // output/Data.Either/index.js
    var Left = /* @__PURE__ */ function() {
        function Left2(value0) {
            this.value0 = value0;
        }
        Left2.create = function(value0) {
            return new Left2(value0);
        };
        return Left2;
    }();
    var Right = /* @__PURE__ */ function() {
        function Right2(value0) {
            this.value0 = value0;
        }
        Right2.create = function(value0) {
            return new Right2(value0);
        };
        return Right2;
    }();
    var either = function(v) {
        return function(v1) {
            return function(v2) {
                if (v2 instanceof Left) return v(v2.value0);
                if (v2 instanceof Right) return v1(v2.value0);
                throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [
                    v.constructor.name,
                    v1.constructor.name,
                    v2.constructor.name
                ]);
            };
        };
    };
    // output/Effect/foreign.js
    var pureE = function(a2) {
        return function() {
            return a2;
        };
    };
    var bindE = function(a2) {
        return function(f) {
            return function() {
                return f(a2())();
            };
        };
    };
    // output/Data.Monoid/index.js
    var mempty = function(dict) {
        return dict.mempty;
    };
    // output/Effect/index.js
    var $runtime_lazy = function(name15, moduleName, init2) {
        var state3 = 0;
        var val;
        return function(lineNumber) {
            if (state3 === 2) return val;
            if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
            state3 = 1;
            val = init2();
            state3 = 2;
            return val;
        };
    };
    var monadEffect = {
        Applicative0: function() {
            return applicativeEffect;
        },
        Bind1: function() {
            return bindEffect;
        }
    };
    var bindEffect = {
        bind: bindE,
        Apply0: function() {
            return $lazy_applyEffect(0);
        }
    };
    var applicativeEffect = {
        pure: pureE,
        Apply0: function() {
            return $lazy_applyEffect(0);
        }
    };
    var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
        return {
            map: liftA1(applicativeEffect)
        };
    });
    var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
        return {
            apply: ap(monadEffect),
            Functor0: function() {
                return $lazy_functorEffect(0);
            }
        };
    });
    var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);
    // output/Effect.Exception/foreign.js
    function error(msg) {
        return new Error(msg);
    }
    function throwException(e) {
        return function() {
            throw e;
        };
    }
    // output/Effect.Exception/index.js
    var $$throw = function($4) {
        return throwException(error($4));
    };
    // output/Control.Monad.Error.Class/index.js
    var throwError = function(dict) {
        return dict.throwError;
    };
    var catchError = function(dict) {
        return dict.catchError;
    };
    var $$try = function(dictMonadError) {
        var catchError1 = catchError(dictMonadError);
        var Monad0 = dictMonadError.MonadThrow0().Monad0();
        var map26 = map(Monad0.Bind1().Apply0().Functor0());
        var pure13 = pure(Monad0.Applicative0());
        return function(a2) {
            return catchError1(map26(Right.create)(a2))(function($52) {
                return pure13(Left.create($52));
            });
        };
    };
    // output/Effect.Ref/foreign.js
    var _new = function(val) {
        return function() {
            return {
                value: val
            };
        };
    };
    var read = function(ref2) {
        return function() {
            return ref2.value;
        };
    };
    var modifyImpl = function(f) {
        return function(ref2) {
            return function() {
                var t = f(ref2.value);
                ref2.value = t.state;
                return t.value;
            };
        };
    };
    var write = function(val) {
        return function(ref2) {
            return function() {
                ref2.value = val;
            };
        };
    };
    // output/Effect.Ref/index.js
    var $$void2 = /* @__PURE__ */ $$void(functorEffect);
    var $$new = _new;
    var modify$prime = modifyImpl;
    var modify = function(f) {
        return modify$prime(function(s) {
            var s$prime = f(s);
            return {
                state: s$prime,
                value: s$prime
            };
        });
    };
    var modify_ = function(f) {
        return function(s) {
            return $$void2(modify(f)(s));
        };
    };
    // output/Control.Monad.Rec.Class/index.js
    var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindEffect);
    var map3 = /* @__PURE__ */ map(functorEffect);
    var Loop = /* @__PURE__ */ function() {
        function Loop2(value0) {
            this.value0 = value0;
        }
        Loop2.create = function(value0) {
            return new Loop2(value0);
        };
        return Loop2;
    }();
    var Done = /* @__PURE__ */ function() {
        function Done2(value0) {
            this.value0 = value0;
        }
        Done2.create = function(value0) {
            return new Done2(value0);
        };
        return Done2;
    }();
    var tailRecM = function(dict) {
        return dict.tailRecM;
    };
    var monadRecEffect = {
        tailRecM: function(f) {
            return function(a2) {
                var fromDone = function(v) {
                    if (v instanceof Done) return v.value0;
                    throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 137, column 30 - line 137, column 44): " + [
                        v.constructor.name
                    ]);
                };
                return function __do2() {
                    var r = bindFlipped2($$new)(f(a2))();
                    (function() {
                        while(!function __do3() {
                            var v = read(r)();
                            if (v instanceof Loop) {
                                var e = f(v.value0)();
                                write(e)(r)();
                                return false;
                            }
                            if (v instanceof Done) return true;
                            throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 128, column 22 - line 133, column 28): " + [
                                v.constructor.name
                            ]);
                        }());
                        return {};
                    })();
                    return map3(fromDone)(read(r))();
                };
            };
        },
        Monad0: function() {
            return monadEffect;
        }
    };
    var forever = function(dictMonadRec) {
        var tailRecM1 = tailRecM(dictMonadRec);
        var voidRight2 = voidRight(dictMonadRec.Monad0().Bind1().Apply0().Functor0());
        return function(ma) {
            return tailRecM1(function(u2) {
                return voidRight2(new Loop(u2))(ma);
            })(unit);
        };
    };
    // output/Unsafe.Coerce/foreign.js
    var unsafeCoerce2 = function(x) {
        return x;
    };
    // output/Data.HeytingAlgebra/foreign.js
    var boolConj = function(b1) {
        return function(b2) {
            return b1 && b2;
        };
    };
    var boolDisj = function(b1) {
        return function(b2) {
            return b1 || b2;
        };
    };
    var boolNot = function(b2) {
        return !b2;
    };
    // output/Data.HeytingAlgebra/index.js
    var tt = function(dict) {
        return dict.tt;
    };
    var not = function(dict) {
        return dict.not;
    };
    var implies = function(dict) {
        return dict.implies;
    };
    var ff = function(dict) {
        return dict.ff;
    };
    var disj = function(dict) {
        return dict.disj;
    };
    var heytingAlgebraBoolean = {
        ff: false,
        tt: true,
        implies: function(a2) {
            return function(b2) {
                return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a2))(b2);
            };
        },
        conj: boolConj,
        disj: boolDisj,
        not: boolNot
    };
    var conj = function(dict) {
        return dict.conj;
    };
    var heytingAlgebraFunction = function(dictHeytingAlgebra) {
        var ff1 = ff(dictHeytingAlgebra);
        var tt1 = tt(dictHeytingAlgebra);
        var implies1 = implies(dictHeytingAlgebra);
        var conj1 = conj(dictHeytingAlgebra);
        var disj1 = disj(dictHeytingAlgebra);
        var not1 = not(dictHeytingAlgebra);
        return {
            ff: function(v) {
                return ff1;
            },
            tt: function(v) {
                return tt1;
            },
            implies: function(f) {
                return function(g) {
                    return function(a2) {
                        return implies1(f(a2))(g(a2));
                    };
                };
            },
            conj: function(f) {
                return function(g) {
                    return function(a2) {
                        return conj1(f(a2))(g(a2));
                    };
                };
            },
            disj: function(f) {
                return function(g) {
                    return function(a2) {
                        return disj1(f(a2))(g(a2));
                    };
                };
            },
            not: function(f) {
                return function(a2) {
                    return not1(f(a2));
                };
            }
        };
    };
    // output/Data.Tuple/index.js
    var Tuple = /* @__PURE__ */ function() {
        function Tuple2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        Tuple2.create = function(value0) {
            return function(value1) {
                return new Tuple2(value0, value1);
            };
        };
        return Tuple2;
    }();
    var snd = function(v) {
        return v.value1;
    };
    var functorTuple = {
        map: function(f) {
            return function(m) {
                return new Tuple(m.value0, f(m.value1));
            };
        }
    };
    var fst = function(v) {
        return v.value0;
    };
    var eqTuple = function(dictEq) {
        var eq2 = eq(dictEq);
        return function(dictEq1) {
            var eq12 = eq(dictEq1);
            return {
                eq: function(x) {
                    return function(y) {
                        return eq2(x.value0)(y.value0) && eq12(x.value1)(y.value1);
                    };
                }
            };
        };
    };
    var ordTuple = function(dictOrd) {
        var compare2 = compare(dictOrd);
        var eqTuple1 = eqTuple(dictOrd.Eq0());
        return function(dictOrd1) {
            var compare12 = compare(dictOrd1);
            var eqTuple2 = eqTuple1(dictOrd1.Eq0());
            return {
                compare: function(x) {
                    return function(y) {
                        var v = compare2(x.value0)(y.value0);
                        if (v instanceof LT) return LT.value;
                        if (v instanceof GT) return GT.value;
                        return compare12(x.value1)(y.value1);
                    };
                },
                Eq0: function() {
                    return eqTuple2;
                }
            };
        };
    };
    // output/Control.Monad.State.Class/index.js
    var state = function(dict) {
        return dict.state;
    };
    var modify_2 = function(dictMonadState) {
        var state1 = state(dictMonadState);
        return function(f) {
            return state1(function(s) {
                return new Tuple(unit, f(s));
            });
        };
    };
    var gets = function(dictMonadState) {
        var state1 = state(dictMonadState);
        return function(f) {
            return state1(function(s) {
                return new Tuple(f(s), s);
            });
        };
    };
    var get = function(dictMonadState) {
        return state(dictMonadState)(function(s) {
            return new Tuple(s, s);
        });
    };
    // output/Control.Monad.Trans.Class/index.js
    var lift = function(dict) {
        return dict.lift;
    };
    // output/Effect.Class/index.js
    var monadEffectEffect = {
        liftEffect: /* @__PURE__ */ identity(categoryFn),
        Monad0: function() {
            return monadEffect;
        }
    };
    var liftEffect = function(dict) {
        return dict.liftEffect;
    };
    // output/Control.Plus/index.js
    var empty = function(dict) {
        return dict.empty;
    };
    // output/Safe.Coerce/index.js
    var coerce = function() {
        return unsafeCoerce2;
    };
    // output/Data.Newtype/index.js
    var coerce2 = /* @__PURE__ */ coerce();
    var unwrap = function() {
        return coerce2;
    };
    var over = function() {
        return function() {
            return function(v) {
                return coerce2;
            };
        };
    };
    // output/Control.Parallel.Class/index.js
    var sequential = function(dict) {
        return dict.sequential;
    };
    var parallel = function(dict) {
        return dict.parallel;
    };
    // output/Data.Foldable/foreign.js
    var foldrArray = function(f) {
        return function(init2) {
            return function(xs) {
                var acc = init2;
                var len = xs.length;
                for(var i2 = len - 1; i2 >= 0; i2--)acc = f(xs[i2])(acc);
                return acc;
            };
        };
    };
    var foldlArray = function(f) {
        return function(init2) {
            return function(xs) {
                var acc = init2;
                var len = xs.length;
                for(var i2 = 0; i2 < len; i2++)acc = f(acc)(xs[i2]);
                return acc;
            };
        };
    };
    // output/Data.Bifunctor/index.js
    var identity4 = /* @__PURE__ */ identity(categoryFn);
    var bimap = function(dict) {
        return dict.bimap;
    };
    var lmap = function(dictBifunctor) {
        var bimap1 = bimap(dictBifunctor);
        return function(f) {
            return bimap1(f)(identity4);
        };
    };
    // output/Data.Foldable/index.js
    var foldr = function(dict) {
        return dict.foldr;
    };
    var traverse_ = function(dictApplicative) {
        var applySecond2 = applySecond(dictApplicative.Apply0());
        var pure13 = pure(dictApplicative);
        return function(dictFoldable) {
            var foldr22 = foldr(dictFoldable);
            return function(f) {
                return foldr22(function($454) {
                    return applySecond2(f($454));
                })(pure13(unit));
            };
        };
    };
    var for_ = function(dictApplicative) {
        var traverse_14 = traverse_(dictApplicative);
        return function(dictFoldable) {
            return flip(traverse_14(dictFoldable));
        };
    };
    var foldl = function(dict) {
        return dict.foldl;
    };
    var foldableMaybe = {
        foldr: function(v) {
            return function(v1) {
                return function(v2) {
                    if (v2 instanceof Nothing) return v1;
                    if (v2 instanceof Just) return v(v2.value0)(v1);
                    throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [
                        v.constructor.name,
                        v1.constructor.name,
                        v2.constructor.name
                    ]);
                };
            };
        },
        foldl: function(v) {
            return function(v1) {
                return function(v2) {
                    if (v2 instanceof Nothing) return v1;
                    if (v2 instanceof Just) return v(v1)(v2.value0);
                    throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [
                        v.constructor.name,
                        v1.constructor.name,
                        v2.constructor.name
                    ]);
                };
            };
        },
        foldMap: function(dictMonoid) {
            var mempty2 = mempty(dictMonoid);
            return function(v) {
                return function(v1) {
                    if (v1 instanceof Nothing) return mempty2;
                    if (v1 instanceof Just) return v(v1.value0);
                    throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [
                        v.constructor.name,
                        v1.constructor.name
                    ]);
                };
            };
        }
    };
    var foldMapDefaultR = function(dictFoldable) {
        var foldr22 = foldr(dictFoldable);
        return function(dictMonoid) {
            var append5 = append(dictMonoid.Semigroup0());
            var mempty2 = mempty(dictMonoid);
            return function(f) {
                return foldr22(function(x) {
                    return function(acc) {
                        return append5(f(x))(acc);
                    };
                })(mempty2);
            };
        };
    };
    var foldableArray = {
        foldr: foldrArray,
        foldl: foldlArray,
        foldMap: function(dictMonoid) {
            return foldMapDefaultR(foldableArray)(dictMonoid);
        }
    };
    // output/Data.Traversable/foreign.js
    var traverseArrayImpl = function() {
        function array1(a2) {
            return [
                a2
            ];
        }
        function array2(a2) {
            return function(b2) {
                return [
                    a2,
                    b2
                ];
            };
        }
        function array3(a2) {
            return function(b2) {
                return function(c) {
                    return [
                        a2,
                        b2,
                        c
                    ];
                };
            };
        }
        function concat2(xs) {
            return function(ys) {
                return xs.concat(ys);
            };
        }
        return function(apply2) {
            return function(map26) {
                return function(pure13) {
                    return function(f) {
                        return function(array) {
                            function go2(bot, top2) {
                                switch(top2 - bot){
                                    case 0:
                                        return pure13([]);
                                    case 1:
                                        return map26(array1)(f(array[bot]));
                                    case 2:
                                        return apply2(map26(array2)(f(array[bot])))(f(array[bot + 1]));
                                    case 3:
                                        return apply2(apply2(map26(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                                    default:
                                        var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                                        return apply2(map26(concat2)(go2(bot, pivot)))(go2(pivot, top2));
                                }
                            }
                            return go2(0, array.length);
                        };
                    };
                };
            };
        };
    }();
    // output/Control.Parallel/index.js
    var identity5 = /* @__PURE__ */ identity(categoryFn);
    var parTraverse_ = function(dictParallel) {
        var sequential3 = sequential(dictParallel);
        var parallel4 = parallel(dictParallel);
        return function(dictApplicative) {
            var traverse_8 = traverse_(dictApplicative);
            return function(dictFoldable) {
                var traverse_14 = traverse_8(dictFoldable);
                return function(f) {
                    var $51 = traverse_14(function($53) {
                        return parallel4(f($53));
                    });
                    return function($52) {
                        return sequential3($51($52));
                    };
                };
            };
        };
    };
    var parSequence_ = function(dictParallel) {
        var parTraverse_1 = parTraverse_(dictParallel);
        return function(dictApplicative) {
            var parTraverse_2 = parTraverse_1(dictApplicative);
            return function(dictFoldable) {
                return parTraverse_2(dictFoldable)(identity5);
            };
        };
    };
    // output/Effect.Unsafe/foreign.js
    var unsafePerformEffect = function(f) {
        return f();
    };
    // output/Partial.Unsafe/foreign.js
    var _unsafePartial = function(f) {
        return f();
    };
    // output/Partial/foreign.js
    var _crashWith = function(msg) {
        throw new Error(msg);
    };
    // output/Partial/index.js
    var crashWith = function() {
        return _crashWith;
    };
    // output/Partial.Unsafe/index.js
    var crashWith2 = /* @__PURE__ */ crashWith();
    var unsafePartial = _unsafePartial;
    var unsafeCrashWith = function(msg) {
        return unsafePartial(function() {
            return crashWith2(msg);
        });
    };
    // output/Effect.Aff/index.js
    var $runtime_lazy2 = function(name15, moduleName, init2) {
        var state3 = 0;
        var val;
        return function(lineNumber) {
            if (state3 === 2) return val;
            if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
            state3 = 1;
            val = init2();
            state3 = 2;
            return val;
        };
    };
    var pure2 = /* @__PURE__ */ pure(applicativeEffect);
    var $$void3 = /* @__PURE__ */ $$void(functorEffect);
    var map4 = /* @__PURE__ */ map(functorEffect);
    var Canceler = function(x) {
        return x;
    };
    var suspendAff = /* @__PURE__ */ _fork(false);
    var functorParAff = {
        map: _parAffMap
    };
    var functorAff = {
        map: _map
    };
    var map1 = /* @__PURE__ */ map(functorAff);
    var forkAff = /* @__PURE__ */ _fork(true);
    var ffiUtil = /* @__PURE__ */ function() {
        var unsafeFromRight = function(v) {
            if (v instanceof Right) return v.value0;
            if (v instanceof Left) return unsafeCrashWith("unsafeFromRight: Left");
            throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): " + [
                v.constructor.name
            ]);
        };
        var unsafeFromLeft = function(v) {
            if (v instanceof Left) return v.value0;
            if (v instanceof Right) return unsafeCrashWith("unsafeFromLeft: Right");
            throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): " + [
                v.constructor.name
            ]);
        };
        var isLeft = function(v) {
            if (v instanceof Left) return true;
            if (v instanceof Right) return false;
            throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): " + [
                v.constructor.name
            ]);
        };
        return {
            isLeft,
            fromLeft: unsafeFromLeft,
            fromRight: unsafeFromRight,
            left: Left.create,
            right: Right.create
        };
    }();
    var makeFiber = function(aff) {
        return _makeFiber(ffiUtil, aff);
    };
    var launchAff = function(aff) {
        return function __do2() {
            var fiber = makeFiber(aff)();
            fiber.run();
            return fiber;
        };
    };
    var bracket = function(acquire) {
        return function(completed) {
            return generalBracket(acquire)({
                killed: $$const(completed),
                failed: $$const(completed),
                completed: $$const(completed)
            });
        };
    };
    var applyParAff = {
        apply: _parAffApply,
        Functor0: function() {
            return functorParAff;
        }
    };
    var monadAff = {
        Applicative0: function() {
            return applicativeAff;
        },
        Bind1: function() {
            return bindAff;
        }
    };
    var bindAff = {
        bind: _bind,
        Apply0: function() {
            return $lazy_applyAff(0);
        }
    };
    var applicativeAff = {
        pure: _pure,
        Apply0: function() {
            return $lazy_applyAff(0);
        }
    };
    var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy2("applyAff", "Effect.Aff", function() {
        return {
            apply: ap(monadAff),
            Functor0: function() {
                return functorAff;
            }
        };
    });
    var applyAff = /* @__PURE__ */ $lazy_applyAff(73);
    var pure22 = /* @__PURE__ */ pure(applicativeAff);
    var bind1 = /* @__PURE__ */ bind(bindAff);
    var bindFlipped3 = /* @__PURE__ */ bindFlipped(bindAff);
    var $$finally = function(fin) {
        return function(a2) {
            return bracket(pure22(unit))($$const(fin))($$const(a2));
        };
    };
    var parallelAff = {
        parallel: unsafeCoerce2,
        sequential: _sequential,
        Apply0: function() {
            return applyAff;
        },
        Apply1: function() {
            return applyParAff;
        }
    };
    var parallel2 = /* @__PURE__ */ parallel(parallelAff);
    var applicativeParAff = {
        pure: function($76) {
            return parallel2(pure22($76));
        },
        Apply0: function() {
            return applyParAff;
        }
    };
    var monadEffectAff = {
        liftEffect: _liftEffect,
        Monad0: function() {
            return monadAff;
        }
    };
    var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
    var effectCanceler = function($77) {
        return Canceler($$const(liftEffect2($77)));
    };
    var joinFiber = function(v) {
        return makeAff(function(k) {
            return map4(effectCanceler)(v.join(k));
        });
    };
    var functorFiber = {
        map: function(f) {
            return function(t) {
                return unsafePerformEffect(makeFiber(map1(f)(joinFiber(t))));
            };
        }
    };
    var killFiber = function(e) {
        return function(v) {
            return bind1(liftEffect2(v.isSuspended))(function(suspended) {
                if (suspended) return liftEffect2($$void3(v.kill(e, $$const(pure2(unit)))));
                return makeAff(function(k) {
                    return map4(effectCanceler)(v.kill(e, k));
                });
            });
        };
    };
    var monadThrowAff = {
        throwError: _throwError,
        Monad0: function() {
            return monadAff;
        }
    };
    var monadErrorAff = {
        catchError: _catchError,
        MonadThrow0: function() {
            return monadThrowAff;
        }
    };
    var $$try2 = /* @__PURE__ */ $$try(monadErrorAff);
    var runAff = function(k) {
        return function(aff) {
            return launchAff(bindFlipped3(function($83) {
                return liftEffect2(k($83));
            })($$try2(aff)));
        };
    };
    var runAff_ = function(k) {
        return function(aff) {
            return $$void3(runAff(k)(aff));
        };
    };
    var monadRecAff = {
        tailRecM: function(k) {
            var go2 = function(a2) {
                return bind1(k(a2))(function(res) {
                    if (res instanceof Done) return pure22(res.value0);
                    if (res instanceof Loop) return go2(res.value0);
                    throw new Error("Failed pattern match at Effect.Aff (line 104, column 7 - line 106, column 23): " + [
                        res.constructor.name
                    ]);
                });
            };
            return go2;
        },
        Monad0: function() {
            return monadAff;
        }
    };
    var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure22(unit));
    // output/Effect.Aff.Class/index.js
    var monadAffAff = {
        liftAff: /* @__PURE__ */ identity(categoryFn),
        MonadEffect0: function() {
            return monadEffectAff;
        }
    };
    var liftAff = function(dict) {
        return dict.liftAff;
    };
    // output/Web.DOM.ParentNode/foreign.js
    var getEffProp = function(name15) {
        return function(node) {
            return function() {
                return node[name15];
            };
        };
    };
    var children = getEffProp("children");
    var _firstElementChild = getEffProp("firstElementChild");
    var _lastElementChild = getEffProp("lastElementChild");
    var childElementCount = getEffProp("childElementCount");
    function _querySelector(selector) {
        return function(node) {
            return function() {
                return node.querySelector(selector);
            };
        };
    }
    // output/Data.Nullable/foreign.js
    var nullImpl = null;
    function nullable(a2, r, f) {
        return a2 == null ? r : f(a2);
    }
    function notNull(x) {
        return x;
    }
    // output/Data.Nullable/index.js
    var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
    var toMaybe = function(n) {
        return nullable(n, Nothing.value, Just.create);
    };
    // output/Web.DOM.ParentNode/index.js
    var map5 = /* @__PURE__ */ map(functorEffect);
    var querySelector = function(qs) {
        var $2 = map5(toMaybe);
        var $3 = _querySelector(qs);
        return function($4) {
            return $2($3($4));
        };
    };
    // output/Web.Event.EventTarget/foreign.js
    function eventListener(fn) {
        return function() {
            return function(event) {
                return fn(event)();
            };
        };
    }
    function addEventListener(type) {
        return function(listener) {
            return function(useCapture) {
                return function(target6) {
                    return function() {
                        return target6.addEventListener(type, listener, useCapture);
                    };
                };
            };
        };
    }
    function removeEventListener(type) {
        return function(listener) {
            return function(useCapture) {
                return function(target6) {
                    return function() {
                        return target6.removeEventListener(type, listener, useCapture);
                    };
                };
            };
        };
    }
    // output/Web.HTML/foreign.js
    var windowImpl = function() {
        return window;
    };
    // output/Web.HTML.HTMLDocument/foreign.js
    function _readyState(doc) {
        return doc.readyState;
    }
    // output/Web.HTML.HTMLDocument.ReadyState/index.js
    var Loading = /* @__PURE__ */ function() {
        function Loading2() {}
        Loading2.value = new Loading2();
        return Loading2;
    }();
    var Interactive = /* @__PURE__ */ function() {
        function Interactive2() {}
        Interactive2.value = new Interactive2();
        return Interactive2;
    }();
    var Complete = /* @__PURE__ */ function() {
        function Complete2() {}
        Complete2.value = new Complete2();
        return Complete2;
    }();
    var parse = function(v) {
        if (v === "loading") return new Just(Loading.value);
        if (v === "interactive") return new Just(Interactive.value);
        if (v === "complete") return new Just(Complete.value);
        return Nothing.value;
    };
    // output/Web.HTML.HTMLDocument/index.js
    var map6 = /* @__PURE__ */ map(functorEffect);
    var toParentNode = unsafeCoerce2;
    var toDocument = unsafeCoerce2;
    var readyState = function(doc) {
        return map6(function() {
            var $4 = fromMaybe(Loading.value);
            return function($5) {
                return $4(parse($5));
            };
        }())(function() {
            return _readyState(doc);
        });
    };
    // output/Web.HTML.HTMLElement/foreign.js
    function _read(nothing, just, value12) {
        var tag = Object.prototype.toString.call(value12);
        if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) return just(value12);
        else return nothing;
    }
    // output/Web.HTML.HTMLElement/index.js
    var toNode = unsafeCoerce2;
    var fromElement = function(x) {
        return _read(Nothing.value, Just.create, x);
    };
    // output/Web.HTML.Window/foreign.js
    function document(window2) {
        return function() {
            return window2.document;
        };
    }
    // output/Web.HTML.Window/index.js
    var toEventTarget = unsafeCoerce2;
    // output/Web.HTML.Event.EventTypes/index.js
    var domcontentloaded = "DOMContentLoaded";
    // output/Halogen.Aff.Util/index.js
    var bind2 = /* @__PURE__ */ bind(bindAff);
    var liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectAff);
    var bindFlipped4 = /* @__PURE__ */ bindFlipped(bindEffect);
    var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(bindEffect);
    var pure3 = /* @__PURE__ */ pure(applicativeAff);
    var bindFlipped1 = /* @__PURE__ */ bindFlipped(bindMaybe);
    var pure1 = /* @__PURE__ */ pure(applicativeEffect);
    var map7 = /* @__PURE__ */ map(functorEffect);
    var discard2 = /* @__PURE__ */ discard(discardUnit);
    var throwError2 = /* @__PURE__ */ throwError(monadThrowAff);
    var selectElement = function(query3) {
        return bind2(liftEffect3(bindFlipped4(composeKleisliFlipped2(function() {
            var $16 = querySelector(query3);
            return function($17) {
                return $16(toParentNode($17));
            };
        }())(document))(windowImpl)))(function(mel) {
            return pure3(bindFlipped1(fromElement)(mel));
        });
    };
    var runHalogenAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure1(unit))));
    var awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
        return function __do2() {
            var rs = bindFlipped4(readyState)(bindFlipped4(document)(windowImpl))();
            if (rs instanceof Loading) {
                var et = map7(toEventTarget)(windowImpl)();
                var listener = eventListener(function(v) {
                    return callback(new Right(unit));
                })();
                addEventListener(domcontentloaded)(listener)(false)(et)();
                return effectCanceler(removeEventListener(domcontentloaded)(listener)(false)(et));
            }
            callback(new Right(unit))();
            return nonCanceler;
        };
    });
    var awaitBody = /* @__PURE__ */ discard2(bindAff)(awaitLoad)(function() {
        return bind2(selectElement("body"))(function(body2) {
            return maybe(throwError2(error("Could not find body")))(pure3)(body2);
        });
    });
    // output/Control.Monad.Fork.Class/index.js
    var monadForkAff = {
        suspend: suspendAff,
        fork: forkAff,
        join: joinFiber,
        Monad0: function() {
            return monadAff;
        },
        Functor1: function() {
            return functorFiber;
        }
    };
    var fork = function(dict) {
        return dict.fork;
    };
    // output/Data.NonEmpty/index.js
    var NonEmpty = /* @__PURE__ */ function() {
        function NonEmpty2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        NonEmpty2.create = function(value0) {
            return function(value1) {
                return new NonEmpty2(value0, value1);
            };
        };
        return NonEmpty2;
    }();
    var singleton2 = function(dictPlus) {
        var empty9 = empty(dictPlus);
        return function(a2) {
            return new NonEmpty(a2, empty9);
        };
    };
    // output/Data.List.Types/index.js
    var Nil = /* @__PURE__ */ function() {
        function Nil2() {}
        Nil2.value = new Nil2();
        return Nil2;
    }();
    var Cons = /* @__PURE__ */ function() {
        function Cons2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        Cons2.create = function(value0) {
            return function(value1) {
                return new Cons2(value0, value1);
            };
        };
        return Cons2;
    }();
    var NonEmptyList = function(x) {
        return x;
    };
    var listMap = function(f) {
        var chunkedRevMap = function($copy_v) {
            return function($copy_v1) {
                var $tco_var_v = $copy_v;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(v, v1) {
                    if (v1 instanceof Cons && v1.value1 instanceof Cons && v1.value1.value1 instanceof Cons) {
                        $tco_var_v = new Cons(v1, v);
                        $copy_v1 = v1.value1.value1.value1;
                        return;
                    }
                    var unrolledMap = function(v2) {
                        if (v2 instanceof Cons && v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil) return new Cons(f(v2.value0), new Cons(f(v2.value1.value0), Nil.value));
                        if (v2 instanceof Cons && v2.value1 instanceof Nil) return new Cons(f(v2.value0), Nil.value);
                        return Nil.value;
                    };
                    var reverseUnrolledMap = function($copy_v2) {
                        return function($copy_v3) {
                            var $tco_var_v2 = $copy_v2;
                            var $tco_done1 = false;
                            var $tco_result2;
                            function $tco_loop2(v2, v3) {
                                if (v2 instanceof Cons && v2.value0 instanceof Cons && v2.value0.value1 instanceof Cons && v2.value0.value1.value1 instanceof Cons) {
                                    $tco_var_v2 = v2.value1;
                                    $copy_v3 = new Cons(f(v2.value0.value0), new Cons(f(v2.value0.value1.value0), new Cons(f(v2.value0.value1.value1.value0), v3)));
                                    return;
                                }
                                $tco_done1 = true;
                                return v3;
                            }
                            while(!$tco_done1)$tco_result2 = $tco_loop2($tco_var_v2, $copy_v3);
                            return $tco_result2;
                        };
                    };
                    $tco_done = true;
                    return reverseUnrolledMap(v)(unrolledMap(v1));
                }
                while(!$tco_done)$tco_result = $tco_loop($tco_var_v, $copy_v1);
                return $tco_result;
            };
        };
        return chunkedRevMap(Nil.value);
    };
    var functorList = {
        map: listMap
    };
    var foldableList = {
        foldr: function(f) {
            return function(b2) {
                var rev3 = function() {
                    var go2 = function($copy_v) {
                        return function($copy_v1) {
                            var $tco_var_v = $copy_v;
                            var $tco_done = false;
                            var $tco_result;
                            function $tco_loop(v, v1) {
                                if (v1 instanceof Nil) {
                                    $tco_done = true;
                                    return v;
                                }
                                if (v1 instanceof Cons) {
                                    $tco_var_v = new Cons(v1.value0, v);
                                    $copy_v1 = v1.value1;
                                    return;
                                }
                                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [
                                    v.constructor.name,
                                    v1.constructor.name
                                ]);
                            }
                            while(!$tco_done)$tco_result = $tco_loop($tco_var_v, $copy_v1);
                            return $tco_result;
                        };
                    };
                    return go2(Nil.value);
                }();
                var $284 = foldl(foldableList)(flip(f))(b2);
                return function($285) {
                    return $284(rev3($285));
                };
            };
        },
        foldl: function(f) {
            var go2 = function($copy_b) {
                return function($copy_v) {
                    var $tco_var_b = $copy_b;
                    var $tco_done1 = false;
                    var $tco_result;
                    function $tco_loop(b2, v) {
                        if (v instanceof Nil) {
                            $tco_done1 = true;
                            return b2;
                        }
                        if (v instanceof Cons) {
                            $tco_var_b = f(b2)(v.value0);
                            $copy_v = v.value1;
                            return;
                        }
                        throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [
                            v.constructor.name
                        ]);
                    }
                    while(!$tco_done1)$tco_result = $tco_loop($tco_var_b, $copy_v);
                    return $tco_result;
                };
            };
            return go2;
        },
        foldMap: function(dictMonoid) {
            var append22 = append(dictMonoid.Semigroup0());
            var mempty2 = mempty(dictMonoid);
            return function(f) {
                return foldl(foldableList)(function(acc) {
                    var $286 = append22(acc);
                    return function($287) {
                        return $286(f($287));
                    };
                })(mempty2);
            };
        }
    };
    var foldr2 = /* @__PURE__ */ foldr(foldableList);
    var semigroupList = {
        append: function(xs) {
            return function(ys) {
                return foldr2(Cons.create)(ys)(xs);
            };
        }
    };
    var append1 = /* @__PURE__ */ append(semigroupList);
    var altList = {
        alt: append1,
        Functor0: function() {
            return functorList;
        }
    };
    var plusList = /* @__PURE__ */ function() {
        return {
            empty: Nil.value,
            Alt0: function() {
                return altList;
            }
        };
    }();
    // output/Data.List/index.js
    var reverse = /* @__PURE__ */ function() {
        var go2 = function($copy_v) {
            return function($copy_v1) {
                var $tco_var_v = $copy_v;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(v, v1) {
                    if (v1 instanceof Nil) {
                        $tco_done = true;
                        return v;
                    }
                    if (v1 instanceof Cons) {
                        $tco_var_v = new Cons(v1.value0, v);
                        $copy_v1 = v1.value1;
                        return;
                    }
                    throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [
                        v.constructor.name,
                        v1.constructor.name
                    ]);
                }
                while(!$tco_done)$tco_result = $tco_loop($tco_var_v, $copy_v1);
                return $tco_result;
            };
        };
        return go2(Nil.value);
    }();
    var $$null = function(v) {
        if (v instanceof Nil) return true;
        return false;
    };
    // output/Data.Map.Internal/index.js
    var $runtime_lazy3 = function(name15, moduleName, init2) {
        var state3 = 0;
        var val;
        return function(lineNumber) {
            if (state3 === 2) return val;
            if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
            state3 = 1;
            val = init2();
            state3 = 2;
            return val;
        };
    };
    var map8 = /* @__PURE__ */ map(functorMaybe);
    var Leaf = /* @__PURE__ */ function() {
        function Leaf2() {}
        Leaf2.value = new Leaf2();
        return Leaf2;
    }();
    var Node = /* @__PURE__ */ function() {
        function Node2(value0, value1, value22, value32, value42, value52) {
            this.value0 = value0;
            this.value1 = value1;
            this.value2 = value22;
            this.value3 = value32;
            this.value4 = value42;
            this.value5 = value52;
        }
        Node2.create = function(value0) {
            return function(value1) {
                return function(value22) {
                    return function(value32) {
                        return function(value42) {
                            return function(value52) {
                                return new Node2(value0, value1, value22, value32, value42, value52);
                            };
                        };
                    };
                };
            };
        };
        return Node2;
    }();
    var Split = /* @__PURE__ */ function() {
        function Split2(value0, value1, value22) {
            this.value0 = value0;
            this.value1 = value1;
            this.value2 = value22;
        }
        Split2.create = function(value0) {
            return function(value1) {
                return function(value22) {
                    return new Split2(value0, value1, value22);
                };
            };
        };
        return Split2;
    }();
    var SplitLast = /* @__PURE__ */ function() {
        function SplitLast2(value0, value1, value22) {
            this.value0 = value0;
            this.value1 = value1;
            this.value2 = value22;
        }
        SplitLast2.create = function(value0) {
            return function(value1) {
                return function(value22) {
                    return new SplitLast2(value0, value1, value22);
                };
            };
        };
        return SplitLast2;
    }();
    var unsafeNode = function(k, v, l, r) {
        if (l instanceof Leaf) {
            if (r instanceof Leaf) return new Node(1, 1, k, v, l, r);
            if (r instanceof Node) return new Node(1 + r.value0 | 0, 1 + r.value1 | 0, k, v, l, r);
            throw new Error("Failed pattern match at Data.Map.Internal (line 680, column 5 - line 684, column 39): " + [
                r.constructor.name
            ]);
        }
        if (l instanceof Node) {
            if (r instanceof Leaf) return new Node(1 + l.value0 | 0, 1 + l.value1 | 0, k, v, l, r);
            if (r instanceof Node) return new Node(1 + function() {
                var $277 = l.value0 > r.value0;
                if ($277) return l.value0;
                return r.value0;
            }() | 0, (1 + l.value1 | 0) + r.value1 | 0, k, v, l, r);
            throw new Error("Failed pattern match at Data.Map.Internal (line 686, column 5 - line 690, column 68): " + [
                r.constructor.name
            ]);
        }
        throw new Error("Failed pattern match at Data.Map.Internal (line 678, column 32 - line 690, column 68): " + [
            l.constructor.name
        ]);
    };
    var singleton3 = function(k) {
        return function(v) {
            return new Node(1, 1, k, v, Leaf.value, Leaf.value);
        };
    };
    var unsafeBalancedNode = /* @__PURE__ */ function() {
        var height8 = function(v) {
            if (v instanceof Leaf) return 0;
            if (v instanceof Node) return v.value0;
            throw new Error("Failed pattern match at Data.Map.Internal (line 735, column 12 - line 737, column 26): " + [
                v.constructor.name
            ]);
        };
        var rotateLeft = function(k, v, l, rk, rv, rl, rr) {
            if (rl instanceof Node && rl.value0 > height8(rr)) return unsafeNode(rl.value2, rl.value3, unsafeNode(k, v, l, rl.value4), unsafeNode(rk, rv, rl.value5, rr));
            return unsafeNode(rk, rv, unsafeNode(k, v, l, rl), rr);
        };
        var rotateRight = function(k, v, lk, lv, ll, lr, r) {
            if (lr instanceof Node && height8(ll) <= lr.value0) return unsafeNode(lr.value2, lr.value3, unsafeNode(lk, lv, ll, lr.value4), unsafeNode(k, v, lr.value5, r));
            return unsafeNode(lk, lv, ll, unsafeNode(k, v, lr, r));
        };
        return function(k, v, l, r) {
            if (l instanceof Leaf) {
                if (r instanceof Leaf) return singleton3(k)(v);
                if (r instanceof Node && r.value0 > 1) return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
                return unsafeNode(k, v, l, r);
            }
            if (l instanceof Node) {
                if (r instanceof Node) {
                    if (r.value0 > (l.value0 + 1 | 0)) return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
                    if (l.value0 > (r.value0 + 1 | 0)) return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
                }
                if (r instanceof Leaf && l.value0 > 1) return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
                return unsafeNode(k, v, l, r);
            }
            throw new Error("Failed pattern match at Data.Map.Internal (line 695, column 40 - line 716, column 34): " + [
                l.constructor.name
            ]);
        };
    }();
    var $lazy_unsafeSplit = /* @__PURE__ */ $runtime_lazy3("unsafeSplit", "Data.Map.Internal", function() {
        return function(comp, k, m) {
            if (m instanceof Leaf) return new Split(Nothing.value, Leaf.value, Leaf.value);
            if (m instanceof Node) {
                var v = comp(k)(m.value2);
                if (v instanceof LT) {
                    var v1 = $lazy_unsafeSplit(771)(comp, k, m.value4);
                    return new Split(v1.value0, v1.value1, unsafeBalancedNode(m.value2, m.value3, v1.value2, m.value5));
                }
                if (v instanceof GT) {
                    var v1 = $lazy_unsafeSplit(774)(comp, k, m.value5);
                    return new Split(v1.value0, unsafeBalancedNode(m.value2, m.value3, m.value4, v1.value1), v1.value2);
                }
                if (v instanceof EQ) return new Split(new Just(m.value3), m.value4, m.value5);
                throw new Error("Failed pattern match at Data.Map.Internal (line 769, column 5 - line 777, column 30): " + [
                    v.constructor.name
                ]);
            }
            throw new Error("Failed pattern match at Data.Map.Internal (line 765, column 34 - line 777, column 30): " + [
                m.constructor.name
            ]);
        };
    });
    var unsafeSplit = /* @__PURE__ */ $lazy_unsafeSplit(764);
    var $lazy_unsafeSplitLast = /* @__PURE__ */ $runtime_lazy3("unsafeSplitLast", "Data.Map.Internal", function() {
        return function(k, v, l, r) {
            if (r instanceof Leaf) return new SplitLast(k, v, l);
            if (r instanceof Node) {
                var v1 = $lazy_unsafeSplitLast(757)(r.value2, r.value3, r.value4, r.value5);
                return new SplitLast(v1.value0, v1.value1, unsafeBalancedNode(k, v, l, v1.value2));
            }
            throw new Error("Failed pattern match at Data.Map.Internal (line 754, column 37 - line 758, column 57): " + [
                r.constructor.name
            ]);
        };
    });
    var unsafeSplitLast = /* @__PURE__ */ $lazy_unsafeSplitLast(753);
    var unsafeJoinNodes = function(v, v1) {
        if (v instanceof Leaf) return v1;
        if (v instanceof Node) {
            var v2 = unsafeSplitLast(v.value2, v.value3, v.value4, v.value5);
            return unsafeBalancedNode(v2.value0, v2.value1, v2.value2, v1);
        }
        throw new Error("Failed pattern match at Data.Map.Internal (line 742, column 25 - line 746, column 38): " + [
            v.constructor.name,
            v1.constructor.name
        ]);
    };
    var pop = function(dictOrd) {
        var compare2 = compare(dictOrd);
        return function(k) {
            return function(m) {
                var v = unsafeSplit(compare2, k, m);
                return map8(function(a2) {
                    return new Tuple(a2, unsafeJoinNodes(v.value1, v.value2));
                })(v.value0);
            };
        };
    };
    var lookup = function(dictOrd) {
        var compare2 = compare(dictOrd);
        return function(k) {
            var go2 = function($copy_v) {
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(v) {
                    if (v instanceof Leaf) {
                        $tco_done = true;
                        return Nothing.value;
                    }
                    if (v instanceof Node) {
                        var v1 = compare2(k)(v.value2);
                        if (v1 instanceof LT) {
                            $copy_v = v.value4;
                            return;
                        }
                        if (v1 instanceof GT) {
                            $copy_v = v.value5;
                            return;
                        }
                        if (v1 instanceof EQ) {
                            $tco_done = true;
                            return new Just(v.value3);
                        }
                        throw new Error("Failed pattern match at Data.Map.Internal (line 281, column 7 - line 284, column 22): " + [
                            v1.constructor.name
                        ]);
                    }
                    throw new Error("Failed pattern match at Data.Map.Internal (line 278, column 8 - line 284, column 22): " + [
                        v.constructor.name
                    ]);
                }
                while(!$tco_done)$tco_result = $tco_loop($copy_v);
                return $tco_result;
            };
            return go2;
        };
    };
    var insert = function(dictOrd) {
        var compare2 = compare(dictOrd);
        return function(k) {
            return function(v) {
                var go2 = function(v1) {
                    if (v1 instanceof Leaf) return singleton3(k)(v);
                    if (v1 instanceof Node) {
                        var v2 = compare2(k)(v1.value2);
                        if (v2 instanceof LT) return unsafeBalancedNode(v1.value2, v1.value3, go2(v1.value4), v1.value5);
                        if (v2 instanceof GT) return unsafeBalancedNode(v1.value2, v1.value3, v1.value4, go2(v1.value5));
                        if (v2 instanceof EQ) return new Node(v1.value0, v1.value1, k, v, v1.value4, v1.value5);
                        throw new Error("Failed pattern match at Data.Map.Internal (line 469, column 7 - line 472, column 35): " + [
                            v2.constructor.name
                        ]);
                    }
                    throw new Error("Failed pattern match at Data.Map.Internal (line 466, column 8 - line 472, column 35): " + [
                        v1.constructor.name
                    ]);
                };
                return go2;
            };
        };
    };
    var foldableMap = {
        foldr: function(f) {
            return function(z) {
                var $lazy_go = $runtime_lazy3("go", "Data.Map.Internal", function() {
                    return function(m$prime, z$prime) {
                        if (m$prime instanceof Leaf) return z$prime;
                        if (m$prime instanceof Node) return $lazy_go(170)(m$prime.value4, f(m$prime.value3)($lazy_go(170)(m$prime.value5, z$prime)));
                        throw new Error("Failed pattern match at Data.Map.Internal (line 167, column 26 - line 170, column 43): " + [
                            m$prime.constructor.name
                        ]);
                    };
                });
                var go2 = $lazy_go(167);
                return function(m) {
                    return go2(m, z);
                };
            };
        },
        foldl: function(f) {
            return function(z) {
                var $lazy_go = $runtime_lazy3("go", "Data.Map.Internal", function() {
                    return function(z$prime, m$prime) {
                        if (m$prime instanceof Leaf) return z$prime;
                        if (m$prime instanceof Node) return $lazy_go(176)(f($lazy_go(176)(z$prime, m$prime.value4))(m$prime.value3), m$prime.value5);
                        throw new Error("Failed pattern match at Data.Map.Internal (line 173, column 26 - line 176, column 43): " + [
                            m$prime.constructor.name
                        ]);
                    };
                });
                var go2 = $lazy_go(173);
                return function(m) {
                    return go2(z, m);
                };
            };
        },
        foldMap: function(dictMonoid) {
            var mempty2 = mempty(dictMonoid);
            var append12 = append(dictMonoid.Semigroup0());
            return function(f) {
                var go2 = function(v) {
                    if (v instanceof Leaf) return mempty2;
                    if (v instanceof Node) return append12(go2(v.value4))(append12(f(v.value3))(go2(v.value5)));
                    throw new Error("Failed pattern match at Data.Map.Internal (line 179, column 10 - line 182, column 28): " + [
                        v.constructor.name
                    ]);
                };
                return go2;
            };
        }
    };
    var empty2 = /* @__PURE__ */ function() {
        return Leaf.value;
    }();
    var $$delete = function(dictOrd) {
        var compare2 = compare(dictOrd);
        return function(k) {
            var go2 = function(v) {
                if (v instanceof Leaf) return Leaf.value;
                if (v instanceof Node) {
                    var v1 = compare2(k)(v.value2);
                    if (v1 instanceof LT) return unsafeBalancedNode(v.value2, v.value3, go2(v.value4), v.value5);
                    if (v1 instanceof GT) return unsafeBalancedNode(v.value2, v.value3, v.value4, go2(v.value5));
                    if (v1 instanceof EQ) return unsafeJoinNodes(v.value4, v.value5);
                    throw new Error("Failed pattern match at Data.Map.Internal (line 496, column 7 - line 499, column 43): " + [
                        v1.constructor.name
                    ]);
                }
                throw new Error("Failed pattern match at Data.Map.Internal (line 493, column 8 - line 499, column 43): " + [
                    v.constructor.name
                ]);
            };
            return go2;
        };
    };
    var alter = function(dictOrd) {
        var compare2 = compare(dictOrd);
        return function(f) {
            return function(k) {
                return function(m) {
                    var v = unsafeSplit(compare2, k, m);
                    var v2 = f(v.value0);
                    if (v2 instanceof Nothing) return unsafeJoinNodes(v.value1, v.value2);
                    if (v2 instanceof Just) return unsafeBalancedNode(k, v2.value0, v.value1, v.value2);
                    throw new Error("Failed pattern match at Data.Map.Internal (line 512, column 3 - line 516, column 41): " + [
                        v2.constructor.name
                    ]);
                };
            };
        };
    };
    // output/Effect.Console/foreign.js
    var log = function(s) {
        return function() {
            console.log(s);
        };
    };
    var warn = function(s) {
        return function() {
            console.warn(s);
        };
    };
    // output/Effect.Console/index.js
    var logShow = function(dictShow) {
        var show2 = show(dictShow);
        return function(a2) {
            return log(show2(a2));
        };
    };
    // output/Data.Exists/index.js
    var runExists = unsafeCoerce2;
    var mkExists = unsafeCoerce2;
    // output/Data.Coyoneda/index.js
    var CoyonedaF = /* @__PURE__ */ function() {
        function CoyonedaF2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        CoyonedaF2.create = function(value0) {
            return function(value1) {
                return new CoyonedaF2(value0, value1);
            };
        };
        return CoyonedaF2;
    }();
    var unCoyoneda = function(f) {
        return function(v) {
            return runExists(function(v1) {
                return f(v1.value0)(v1.value1);
            })(v);
        };
    };
    var coyoneda = function(k) {
        return function(fi) {
            return mkExists(new CoyonedaF(k, fi));
        };
    };
    var functorCoyoneda = {
        map: function(f) {
            return function(v) {
                return runExists(function(v1) {
                    return coyoneda(function($180) {
                        return f(v1.value0($180));
                    })(v1.value1);
                })(v);
            };
        }
    };
    var liftCoyoneda = /* @__PURE__ */ coyoneda(/* @__PURE__ */ identity(categoryFn));
    // output/Halogen.Data.OrdBox/index.js
    var OrdBox = /* @__PURE__ */ function() {
        function OrdBox2(value0, value1, value22) {
            this.value0 = value0;
            this.value1 = value1;
            this.value2 = value22;
        }
        OrdBox2.create = function(value0) {
            return function(value1) {
                return function(value22) {
                    return new OrdBox2(value0, value1, value22);
                };
            };
        };
        return OrdBox2;
    }();
    var mkOrdBox = function(dictOrd) {
        return OrdBox.create(eq(dictOrd.Eq0()))(compare(dictOrd));
    };
    var eqOrdBox = {
        eq: function(v) {
            return function(v1) {
                return v.value0(v.value2)(v1.value2);
            };
        }
    };
    var ordOrdBox = {
        compare: function(v) {
            return function(v1) {
                return v.value1(v.value2)(v1.value2);
            };
        },
        Eq0: function() {
            return eqOrdBox;
        }
    };
    // output/Halogen.Data.Slot/index.js
    var ordTuple2 = /* @__PURE__ */ ordTuple(ordString)(ordOrdBox);
    var pop1 = /* @__PURE__ */ pop(ordTuple2);
    var lookup1 = /* @__PURE__ */ lookup(ordTuple2);
    var insert1 = /* @__PURE__ */ insert(ordTuple2);
    var pop2 = function() {
        return function(dictIsSymbol) {
            var reflectSymbol2 = reflectSymbol(dictIsSymbol);
            return function(dictOrd) {
                var mkOrdBox2 = mkOrdBox(dictOrd);
                return function(sym) {
                    return function(key) {
                        return function(v) {
                            return pop1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key)))(v);
                        };
                    };
                };
            };
        };
    };
    var lookup2 = function() {
        return function(dictIsSymbol) {
            var reflectSymbol2 = reflectSymbol(dictIsSymbol);
            return function(dictOrd) {
                var mkOrdBox2 = mkOrdBox(dictOrd);
                return function(sym) {
                    return function(key) {
                        return function(v) {
                            return lookup1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key)))(v);
                        };
                    };
                };
            };
        };
    };
    var insert2 = function() {
        return function(dictIsSymbol) {
            var reflectSymbol2 = reflectSymbol(dictIsSymbol);
            return function(dictOrd) {
                var mkOrdBox2 = mkOrdBox(dictOrd);
                return function(sym) {
                    return function(key) {
                        return function(val) {
                            return function(v) {
                                return insert1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key)))(val)(v);
                            };
                        };
                    };
                };
            };
        };
    };
    var foreachSlot = function(dictApplicative) {
        var traverse_8 = traverse_(dictApplicative)(foldableMap);
        return function(v) {
            return function(k) {
                return traverse_8(function($54) {
                    return k($54);
                })(v);
            };
        };
    };
    var empty3 = empty2;
    // output/Halogen.Query.Input/index.js
    var RefUpdate = /* @__PURE__ */ function() {
        function RefUpdate2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        RefUpdate2.create = function(value0) {
            return function(value1) {
                return new RefUpdate2(value0, value1);
            };
        };
        return RefUpdate2;
    }();
    var Action = /* @__PURE__ */ function() {
        function Action3(value0) {
            this.value0 = value0;
        }
        Action3.create = function(value0) {
            return new Action3(value0);
        };
        return Action3;
    }();
    var functorInput = {
        map: function(f) {
            return function(m) {
                if (m instanceof RefUpdate) return new RefUpdate(m.value0, m.value1);
                if (m instanceof Action) return new Action(f(m.value0));
                throw new Error("Failed pattern match at Halogen.Query.Input (line 0, column 0 - line 0, column 0): " + [
                    m.constructor.name
                ]);
            };
        }
    };
    // output/Data.Array/foreign.js
    var replicateFill = function(count, value12) {
        if (count < 1) return [];
        var result = new Array(count);
        return result.fill(value12);
    };
    var replicatePolyfill = function(count, value12) {
        var result = [];
        var n = 0;
        for(var i2 = 0; i2 < count; i2++)result[n++] = value12;
        return result;
    };
    var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
    var fromFoldableImpl = function() {
        function Cons2(head2, tail) {
            this.head = head2;
            this.tail = tail;
        }
        var emptyList = {};
        function curryCons(head2) {
            return function(tail) {
                return new Cons2(head2, tail);
            };
        }
        function listToArray(list) {
            var result = [];
            var count = 0;
            var xs = list;
            while(xs !== emptyList){
                result[count++] = xs.head;
                xs = xs.tail;
            }
            return result;
        }
        return function(foldr4, xs) {
            return listToArray(foldr4(curryCons)(emptyList)(xs));
        };
    }();
    var length3 = function(xs) {
        return xs.length;
    };
    var findIndexImpl = function(just, nothing, f, xs) {
        for(var i2 = 0, l = xs.length; i2 < l; i2++){
            if (f(xs[i2])) return just(i2);
        }
        return nothing;
    };
    var _deleteAt = function(just, nothing, i2, l) {
        if (i2 < 0 || i2 >= l.length) return nothing;
        var l1 = l.slice();
        l1.splice(i2, 1);
        return just(l1);
    };
    var sortByImpl = function() {
        function mergeFromTo(compare2, fromOrdering, xs1, xs2, from2, to) {
            var mid;
            var i2;
            var j;
            var k;
            var x;
            var y;
            var c;
            mid = from2 + (to - from2 >> 1);
            if (mid - from2 > 1) mergeFromTo(compare2, fromOrdering, xs2, xs1, from2, mid);
            if (to - mid > 1) mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
            i2 = from2;
            j = mid;
            k = from2;
            while(i2 < mid && j < to){
                x = xs2[i2];
                y = xs2[j];
                c = fromOrdering(compare2(x)(y));
                if (c > 0) {
                    xs1[k++] = y;
                    ++j;
                } else {
                    xs1[k++] = x;
                    ++i2;
                }
            }
            while(i2 < mid)xs1[k++] = xs2[i2++];
            while(j < to)xs1[k++] = xs2[j++];
        }
        return function(compare2, fromOrdering, xs) {
            var out;
            if (xs.length < 2) return xs;
            out = xs.slice(0);
            mergeFromTo(compare2, fromOrdering, out, xs.slice(0), 0, xs.length);
            return out;
        };
    }();
    // output/Data.Array.ST/foreign.js
    var sortByImpl2 = function() {
        function mergeFromTo(compare2, fromOrdering, xs1, xs2, from2, to) {
            var mid;
            var i2;
            var j;
            var k;
            var x;
            var y;
            var c;
            mid = from2 + (to - from2 >> 1);
            if (mid - from2 > 1) mergeFromTo(compare2, fromOrdering, xs2, xs1, from2, mid);
            if (to - mid > 1) mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
            i2 = from2;
            j = mid;
            k = from2;
            while(i2 < mid && j < to){
                x = xs2[i2];
                y = xs2[j];
                c = fromOrdering(compare2(x)(y));
                if (c > 0) {
                    xs1[k++] = y;
                    ++j;
                } else {
                    xs1[k++] = x;
                    ++i2;
                }
            }
            while(i2 < mid)xs1[k++] = xs2[i2++];
            while(j < to)xs1[k++] = xs2[j++];
        }
        return function(compare2, fromOrdering, xs) {
            if (xs.length < 2) return xs;
            mergeFromTo(compare2, fromOrdering, xs, xs.slice(0), 0, xs.length);
            return xs;
        };
    }();
    // output/Data.Function.Uncurried/foreign.js
    var runFn4 = function(fn) {
        return function(a2) {
            return function(b2) {
                return function(c) {
                    return function(d) {
                        return fn(a2, b2, c, d);
                    };
                };
            };
        };
    };
    // output/Data.Array/index.js
    var fromJust2 = /* @__PURE__ */ fromJust();
    var findIndex = /* @__PURE__ */ function() {
        return runFn4(findIndexImpl)(Just.create)(Nothing.value);
    }();
    var deleteAt = /* @__PURE__ */ function() {
        return runFn4(_deleteAt)(Just.create)(Nothing.value);
    }();
    var deleteBy = function(v) {
        return function(v1) {
            return function(v2) {
                if (v2.length === 0) return [];
                return maybe(v2)(function(i2) {
                    return fromJust2(deleteAt(i2)(v2));
                })(findIndex(v(v1))(v2));
            };
        };
    };
    // output/Halogen.VDom.Machine/index.js
    var Step = /* @__PURE__ */ function() {
        function Step3(value0, value1, value22, value32) {
            this.value0 = value0;
            this.value1 = value1;
            this.value2 = value22;
            this.value3 = value32;
        }
        Step3.create = function(value0) {
            return function(value1) {
                return function(value22) {
                    return function(value32) {
                        return new Step3(value0, value1, value22, value32);
                    };
                };
            };
        };
        return Step3;
    }();
    var unStep = unsafeCoerce2;
    var step2 = function(v, a2) {
        return v.value2(v.value1, a2);
    };
    var mkStep = unsafeCoerce2;
    var halt = function(v) {
        return v.value3(v.value1);
    };
    var extract2 = /* @__PURE__ */ unStep(function(v) {
        return v.value0;
    });
    // output/Halogen.VDom.Types/index.js
    var map9 = /* @__PURE__ */ map(functorArray);
    var map12 = /* @__PURE__ */ map(functorTuple);
    var Text = /* @__PURE__ */ function() {
        function Text2(value0) {
            this.value0 = value0;
        }
        Text2.create = function(value0) {
            return new Text2(value0);
        };
        return Text2;
    }();
    var Elem = /* @__PURE__ */ function() {
        function Elem2(value0, value1, value22, value32) {
            this.value0 = value0;
            this.value1 = value1;
            this.value2 = value22;
            this.value3 = value32;
        }
        Elem2.create = function(value0) {
            return function(value1) {
                return function(value22) {
                    return function(value32) {
                        return new Elem2(value0, value1, value22, value32);
                    };
                };
            };
        };
        return Elem2;
    }();
    var Keyed = /* @__PURE__ */ function() {
        function Keyed2(value0, value1, value22, value32) {
            this.value0 = value0;
            this.value1 = value1;
            this.value2 = value22;
            this.value3 = value32;
        }
        Keyed2.create = function(value0) {
            return function(value1) {
                return function(value22) {
                    return function(value32) {
                        return new Keyed2(value0, value1, value22, value32);
                    };
                };
            };
        };
        return Keyed2;
    }();
    var Widget = /* @__PURE__ */ function() {
        function Widget2(value0) {
            this.value0 = value0;
        }
        Widget2.create = function(value0) {
            return new Widget2(value0);
        };
        return Widget2;
    }();
    var Grafted = /* @__PURE__ */ function() {
        function Grafted2(value0) {
            this.value0 = value0;
        }
        Grafted2.create = function(value0) {
            return new Grafted2(value0);
        };
        return Grafted2;
    }();
    var Graft = /* @__PURE__ */ function() {
        function Graft2(value0, value1, value22) {
            this.value0 = value0;
            this.value1 = value1;
            this.value2 = value22;
        }
        Graft2.create = function(value0) {
            return function(value1) {
                return function(value22) {
                    return new Graft2(value0, value1, value22);
                };
            };
        };
        return Graft2;
    }();
    var unGraft = function(f) {
        return function($61) {
            return f($61);
        };
    };
    var graft = unsafeCoerce2;
    var bifunctorGraft = {
        bimap: function(f) {
            return function(g) {
                return unGraft(function(v) {
                    return graft(new Graft(function($63) {
                        return f(v.value0($63));
                    }, function($64) {
                        return g(v.value1($64));
                    }, v.value2));
                });
            };
        }
    };
    var bimap2 = /* @__PURE__ */ bimap(bifunctorGraft);
    var bifunctorVDom = {
        bimap: function(v) {
            return function(v1) {
                return function(v2) {
                    if (v2 instanceof Text) return new Text(v2.value0);
                    if (v2 instanceof Grafted) return new Grafted(bimap2(v)(v1)(v2.value0));
                    return new Grafted(graft(new Graft(v, v1, v2)));
                };
            };
        }
    };
    var runGraft = /* @__PURE__ */ unGraft(function(v) {
        var go2 = function(v2) {
            if (v2 instanceof Text) return new Text(v2.value0);
            if (v2 instanceof Elem) return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map9(go2)(v2.value3));
            if (v2 instanceof Keyed) return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map9(map12(go2))(v2.value3));
            if (v2 instanceof Widget) return new Widget(v.value1(v2.value0));
            if (v2 instanceof Grafted) return new Grafted(bimap2(v.value0)(v.value1)(v2.value0));
            throw new Error("Failed pattern match at Halogen.VDom.Types (line 86, column 7 - line 86, column 27): " + [
                v2.constructor.name
            ]);
        };
        return go2(v.value2);
    });
    // output/Halogen.VDom.Util/foreign.js
    function unsafeGetAny(key, obj) {
        return obj[key];
    }
    function unsafeHasAny(key, obj) {
        return obj.hasOwnProperty(key);
    }
    function unsafeSetAny(key, val, obj) {
        obj[key] = val;
    }
    function forE2(a2, f) {
        var b2 = [];
        for(var i2 = 0; i2 < a2.length; i2++)b2.push(f(i2, a2[i2]));
        return b2;
    }
    function forEachE(a2, f) {
        for(var i2 = 0; i2 < a2.length; i2++)f(a2[i2]);
    }
    function forInE(o, f) {
        var ks = Object.keys(o);
        for(var i2 = 0; i2 < ks.length; i2++){
            var k = ks[i2];
            f(k, o[k]);
        }
    }
    function diffWithIxE(a1, a2, f1, f2, f3) {
        var a3 = [];
        var l1 = a1.length;
        var l2 = a2.length;
        var i2 = 0;
        while(true){
            if (i2 < l1) {
                if (i2 < l2) a3.push(f1(i2, a1[i2], a2[i2]));
                else f2(i2, a1[i2]);
            } else if (i2 < l2) a3.push(f3(i2, a2[i2]));
            else break;
            i2++;
        }
        return a3;
    }
    function strMapWithIxE(as, fk, f) {
        var o = {};
        for(var i2 = 0; i2 < as.length; i2++){
            var a2 = as[i2];
            var k = fk(a2);
            o[k] = f(k, i2, a2);
        }
        return o;
    }
    function diffWithKeyAndIxE(o1, as, fk, f1, f2, f3) {
        var o2 = {};
        for(var i2 = 0; i2 < as.length; i2++){
            var a2 = as[i2];
            var k = fk(a2);
            if (o1.hasOwnProperty(k)) o2[k] = f1(k, i2, o1[k], a2);
            else o2[k] = f3(k, i2, a2);
        }
        for(var k in o1){
            if (k in o2) continue;
            f2(k, o1[k]);
        }
        return o2;
    }
    function refEq2(a2, b2) {
        return a2 === b2;
    }
    function createTextNode(s, doc) {
        return doc.createTextNode(s);
    }
    function setTextContent(s, n) {
        n.textContent = s;
    }
    function createElement(ns, name15, doc) {
        if (ns != null) return doc.createElementNS(ns, name15);
        else return doc.createElement(name15);
    }
    function insertChildIx(i2, a2, b2) {
        var n = b2.childNodes.item(i2) || null;
        if (n !== a2) b2.insertBefore(a2, n);
    }
    function removeChild(a2, b2) {
        if (b2 && a2.parentNode === b2) b2.removeChild(a2);
    }
    function parentNode(a2) {
        return a2.parentNode;
    }
    function setAttribute(ns, attr3, val, el) {
        if (ns != null) el.setAttributeNS(ns, attr3, val);
        else el.setAttribute(attr3, val);
    }
    function removeAttribute(ns, attr3, el) {
        if (ns != null) el.removeAttributeNS(ns, attr3);
        else el.removeAttribute(attr3);
    }
    function hasAttribute(ns, attr3, el) {
        if (ns != null) return el.hasAttributeNS(ns, attr3);
        else return el.hasAttribute(attr3);
    }
    function addEventListener2(ev, listener, el) {
        el.addEventListener(ev, listener, false);
    }
    function removeEventListener2(ev, listener, el) {
        el.removeEventListener(ev, listener, false);
    }
    var jsUndefined = void 0;
    // output/Foreign.Object.ST/foreign.js
    var newImpl = function() {
        return {};
    };
    // output/Halogen.VDom.Util/index.js
    var unsafeLookup = unsafeGetAny;
    var unsafeFreeze2 = unsafeCoerce2;
    var pokeMutMap = unsafeSetAny;
    var newMutMap = newImpl;
    // output/Web.DOM.Element/foreign.js
    var getProp = function(name15) {
        return function(doctype) {
            return doctype[name15];
        };
    };
    var _namespaceURI = getProp("namespaceURI");
    var _prefix = getProp("prefix");
    var localName = getProp("localName");
    var tagName = getProp("tagName");
    // output/Web.DOM.Element/index.js
    var toNode2 = unsafeCoerce2;
    // output/Halogen.VDom.DOM/index.js
    var $runtime_lazy4 = function(name15, moduleName, init2) {
        var state3 = 0;
        var val;
        return function(lineNumber) {
            if (state3 === 2) return val;
            if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
            state3 = 1;
            val = init2();
            state3 = 2;
            return val;
        };
    };
    var haltWidget = function(v) {
        return halt(v.widget);
    };
    var $lazy_patchWidget = /* @__PURE__ */ $runtime_lazy4("patchWidget", "Halogen.VDom.DOM", function() {
        return function(state3, vdom) {
            if (vdom instanceof Grafted) return $lazy_patchWidget(291)(state3, runGraft(vdom.value0));
            if (vdom instanceof Widget) {
                var res = step2(state3.widget, vdom.value0);
                var res$prime = unStep(function(v) {
                    return mkStep(new Step(v.value0, {
                        build: state3.build,
                        widget: res
                    }, $lazy_patchWidget(296), haltWidget));
                })(res);
                return res$prime;
            }
            haltWidget(state3);
            return state3.build(vdom);
        };
    });
    var patchWidget = /* @__PURE__ */ $lazy_patchWidget(286);
    var haltText = function(v) {
        var parent2 = parentNode(v.node);
        return removeChild(v.node, parent2);
    };
    var $lazy_patchText = /* @__PURE__ */ $runtime_lazy4("patchText", "Halogen.VDom.DOM", function() {
        return function(state3, vdom) {
            if (vdom instanceof Grafted) return $lazy_patchText(82)(state3, runGraft(vdom.value0));
            if (vdom instanceof Text) {
                if (state3.value === vdom.value0) return mkStep(new Step(state3.node, state3, $lazy_patchText(85), haltText));
                if (otherwise) {
                    var nextState = {
                        build: state3.build,
                        node: state3.node,
                        value: vdom.value0
                    };
                    setTextContent(vdom.value0, state3.node);
                    return mkStep(new Step(state3.node, nextState, $lazy_patchText(89), haltText));
                }
            }
            haltText(state3);
            return state3.build(vdom);
        };
    });
    var patchText = /* @__PURE__ */ $lazy_patchText(77);
    var haltKeyed = function(v) {
        var parent2 = parentNode(v.node);
        removeChild(v.node, parent2);
        forInE(v.children, function(v1, s) {
            return halt(s);
        });
        return halt(v.attrs);
    };
    var haltElem = function(v) {
        var parent2 = parentNode(v.node);
        removeChild(v.node, parent2);
        forEachE(v.children, halt);
        return halt(v.attrs);
    };
    var eqElemSpec = function(ns1, v, ns2, v1) {
        var $63 = v === v1;
        if ($63) {
            if (ns1 instanceof Just && ns2 instanceof Just && ns1.value0 === ns2.value0) return true;
            if (ns1 instanceof Nothing && ns2 instanceof Nothing) return true;
            return false;
        }
        return false;
    };
    var $lazy_patchElem = /* @__PURE__ */ $runtime_lazy4("patchElem", "Halogen.VDom.DOM", function() {
        return function(state3, vdom) {
            if (vdom instanceof Grafted) return $lazy_patchElem(135)(state3, runGraft(vdom.value0));
            if (vdom instanceof Elem && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
                var v = length3(vdom.value3);
                var v1 = length3(state3.children);
                if (v1 === 0 && v === 0) {
                    var attrs2 = step2(state3.attrs, vdom.value2);
                    var nextState = {
                        build: state3.build,
                        node: state3.node,
                        attrs: attrs2,
                        ns: vdom.value0,
                        name: vdom.value1,
                        children: state3.children
                    };
                    return mkStep(new Step(state3.node, nextState, $lazy_patchElem(149), haltElem));
                }
                var onThis = function(v2, s) {
                    return halt(s);
                };
                var onThese = function(ix, s, v2) {
                    var res = step2(s, v2);
                    insertChildIx(ix, extract2(res), state3.node);
                    return res;
                };
                var onThat = function(ix, v2) {
                    var res = state3.build(v2);
                    insertChildIx(ix, extract2(res), state3.node);
                    return res;
                };
                var children2 = diffWithIxE(state3.children, vdom.value3, onThese, onThis, onThat);
                var attrs2 = step2(state3.attrs, vdom.value2);
                var nextState = {
                    build: state3.build,
                    node: state3.node,
                    attrs: attrs2,
                    ns: vdom.value0,
                    name: vdom.value1,
                    children: children2
                };
                return mkStep(new Step(state3.node, nextState, $lazy_patchElem(172), haltElem));
            }
            haltElem(state3);
            return state3.build(vdom);
        };
    });
    var patchElem = /* @__PURE__ */ $lazy_patchElem(130);
    var $lazy_patchKeyed = /* @__PURE__ */ $runtime_lazy4("patchKeyed", "Halogen.VDom.DOM", function() {
        return function(state3, vdom) {
            if (vdom instanceof Grafted) return $lazy_patchKeyed(222)(state3, runGraft(vdom.value0));
            if (vdom instanceof Keyed && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
                var v = length3(vdom.value3);
                if (state3.length === 0 && v === 0) {
                    var attrs2 = step2(state3.attrs, vdom.value2);
                    var nextState = {
                        build: state3.build,
                        node: state3.node,
                        attrs: attrs2,
                        ns: vdom.value0,
                        name: vdom.value1,
                        children: state3.children,
                        length: 0
                    };
                    return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(237), haltKeyed));
                }
                var onThis = function(v2, s) {
                    return halt(s);
                };
                var onThese = function(v2, ix$prime, s, v3) {
                    var res = step2(s, v3.value1);
                    insertChildIx(ix$prime, extract2(res), state3.node);
                    return res;
                };
                var onThat = function(v2, ix, v3) {
                    var res = state3.build(v3.value1);
                    insertChildIx(ix, extract2(res), state3.node);
                    return res;
                };
                var children2 = diffWithKeyAndIxE(state3.children, vdom.value3, fst, onThese, onThis, onThat);
                var attrs2 = step2(state3.attrs, vdom.value2);
                var nextState = {
                    build: state3.build,
                    node: state3.node,
                    attrs: attrs2,
                    ns: vdom.value0,
                    name: vdom.value1,
                    children: children2,
                    length: v
                };
                return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(261), haltKeyed));
            }
            haltKeyed(state3);
            return state3.build(vdom);
        };
    });
    var patchKeyed = /* @__PURE__ */ $lazy_patchKeyed(217);
    var buildWidget = function(v, build, w) {
        var res = v.buildWidget(v)(w);
        var res$prime = unStep(function(v1) {
            return mkStep(new Step(v1.value0, {
                build,
                widget: res
            }, patchWidget, haltWidget));
        })(res);
        return res$prime;
    };
    var buildText = function(v, build, s) {
        var node = createTextNode(s, v.document);
        var state3 = {
            build,
            node,
            value: s
        };
        return mkStep(new Step(node, state3, patchText, haltText));
    };
    var buildKeyed = function(v, build, ns1, name1, as1, ch1) {
        var el = createElement(toNullable(ns1), name1, v.document);
        var node = toNode2(el);
        var onChild = function(v1, ix, v2) {
            var res = build(v2.value1);
            insertChildIx(ix, extract2(res), node);
            return res;
        };
        var children2 = strMapWithIxE(ch1, fst, onChild);
        var attrs = v.buildAttributes(el)(as1);
        var state3 = {
            build,
            node,
            attrs,
            ns: ns1,
            name: name1,
            children: children2,
            length: length3(ch1)
        };
        return mkStep(new Step(node, state3, patchKeyed, haltKeyed));
    };
    var buildElem = function(v, build, ns1, name1, as1, ch1) {
        var el = createElement(toNullable(ns1), name1, v.document);
        var node = toNode2(el);
        var onChild = function(ix, child) {
            var res = build(child);
            insertChildIx(ix, extract2(res), node);
            return res;
        };
        var children2 = forE2(ch1, onChild);
        var attrs = v.buildAttributes(el)(as1);
        var state3 = {
            build,
            node,
            attrs,
            ns: ns1,
            name: name1,
            children: children2
        };
        return mkStep(new Step(node, state3, patchElem, haltElem));
    };
    var buildVDom = function(spec) {
        var $lazy_build = $runtime_lazy4("build", "Halogen.VDom.DOM", function() {
            return function(v) {
                if (v instanceof Text) return buildText(spec, $lazy_build(59), v.value0);
                if (v instanceof Elem) return buildElem(spec, $lazy_build(60), v.value0, v.value1, v.value2, v.value3);
                if (v instanceof Keyed) return buildKeyed(spec, $lazy_build(61), v.value0, v.value1, v.value2, v.value3);
                if (v instanceof Widget) return buildWidget(spec, $lazy_build(62), v.value0);
                if (v instanceof Grafted) return $lazy_build(63)(runGraft(v.value0));
                throw new Error("Failed pattern match at Halogen.VDom.DOM (line 58, column 27 - line 63, column 52): " + [
                    v.constructor.name
                ]);
            };
        });
        var build = $lazy_build(58);
        return build;
    };
    // output/Foreign/foreign.js
    function typeOf(value12) {
        return typeof value12;
    }
    var isArray = Array.isArray || function(value12) {
        return Object.prototype.toString.call(value12) === "[object Array]";
    };
    // output/Data.List.NonEmpty/index.js
    var singleton4 = /* @__PURE__ */ function() {
        var $200 = singleton2(plusList);
        return function($201) {
            return NonEmptyList($200($201));
        };
    }();
    var cons = function(y) {
        return function(v) {
            return new NonEmpty(y, new Cons(v.value0, v.value1));
        };
    };
    // output/Foreign.Object/foreign.js
    function _lookup(no, yes, k, m) {
        return k in m ? yes(m[k]) : no;
    }
    function toArrayWithKey(f) {
        return function(m) {
            var r = [];
            for(var k in m)if (hasOwnProperty.call(m, k)) r.push(f(k)(m[k]));
            return r;
        };
    }
    var keys = Object.keys || toArrayWithKey(function(k) {
        return function() {
            return k;
        };
    });
    // output/Foreign.Object/index.js
    var lookup3 = /* @__PURE__ */ function() {
        return runFn4(_lookup)(Nothing.value)(Just.create);
    }();
    // output/Halogen.VDom.DOM.Prop/index.js
    var $runtime_lazy5 = function(name15, moduleName, init2) {
        var state3 = 0;
        var val;
        return function(lineNumber) {
            if (state3 === 2) return val;
            if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
            state3 = 1;
            val = init2();
            state3 = 2;
            return val;
        };
    };
    var map10 = /* @__PURE__ */ map(functorFn);
    var map13 = /* @__PURE__ */ map(functorMaybe);
    var Created = /* @__PURE__ */ function() {
        function Created2(value0) {
            this.value0 = value0;
        }
        Created2.create = function(value0) {
            return new Created2(value0);
        };
        return Created2;
    }();
    var Removed = /* @__PURE__ */ function() {
        function Removed2(value0) {
            this.value0 = value0;
        }
        Removed2.create = function(value0) {
            return new Removed2(value0);
        };
        return Removed2;
    }();
    var Attribute = /* @__PURE__ */ function() {
        function Attribute2(value0, value1, value22) {
            this.value0 = value0;
            this.value1 = value1;
            this.value2 = value22;
        }
        Attribute2.create = function(value0) {
            return function(value1) {
                return function(value22) {
                    return new Attribute2(value0, value1, value22);
                };
            };
        };
        return Attribute2;
    }();
    var Property = /* @__PURE__ */ function() {
        function Property2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        Property2.create = function(value0) {
            return function(value1) {
                return new Property2(value0, value1);
            };
        };
        return Property2;
    }();
    var Handler = /* @__PURE__ */ function() {
        function Handler2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        Handler2.create = function(value0) {
            return function(value1) {
                return new Handler2(value0, value1);
            };
        };
        return Handler2;
    }();
    var Ref = /* @__PURE__ */ function() {
        function Ref2(value0) {
            this.value0 = value0;
        }
        Ref2.create = function(value0) {
            return new Ref2(value0);
        };
        return Ref2;
    }();
    var unsafeGetProperty = unsafeGetAny;
    var setProperty = unsafeSetAny;
    var removeProperty = function(key, el) {
        var v = hasAttribute(nullImpl, key, el);
        if (v) return removeAttribute(nullImpl, key, el);
        var v1 = typeOf(unsafeGetAny(key, el));
        if (v1 === "string") return unsafeSetAny(key, "", el);
        if (key === "rowSpan") return unsafeSetAny(key, 1, el);
        if (key === "colSpan") return unsafeSetAny(key, 1, el);
        return unsafeSetAny(key, jsUndefined, el);
    };
    var propToStrKey = function(v) {
        if (v instanceof Attribute && v.value0 instanceof Just) return "attr/" + (v.value0.value0 + (":" + v.value1));
        if (v instanceof Attribute) return "attr/:" + v.value1;
        if (v instanceof Property) return "prop/" + v.value0;
        if (v instanceof Handler) return "handler/" + v.value0;
        if (v instanceof Ref) return "ref";
        throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 182, column 16 - line 187, column 16): " + [
            v.constructor.name
        ]);
    };
    var functorProp = {
        map: function(v) {
            return function(v1) {
                if (v1 instanceof Handler) return new Handler(v1.value0, map10(map13(v))(v1.value1));
                if (v1 instanceof Ref) return new Ref(map10(map13(v))(v1.value0));
                return v1;
            };
        }
    };
    var buildProp = function(emit) {
        return function(el) {
            var removeProp = function(prevEvents) {
                return function(v, v1) {
                    if (v1 instanceof Attribute) return removeAttribute(toNullable(v1.value0), v1.value1, el);
                    if (v1 instanceof Property) return removeProperty(v1.value0, el);
                    if (v1 instanceof Handler) {
                        var handler3 = unsafeLookup(v1.value0, prevEvents);
                        return removeEventListener2(v1.value0, fst(handler3), el);
                    }
                    if (v1 instanceof Ref) return unit;
                    throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 169, column 5 - line 179, column 18): " + [
                        v1.constructor.name
                    ]);
                };
            };
            var mbEmit = function(v) {
                if (v instanceof Just) return emit(v.value0)();
                return unit;
            };
            var haltProp = function(state3) {
                var v = lookup3("ref")(state3.props);
                if (v instanceof Just && v.value0 instanceof Ref) return mbEmit(v.value0.value0(new Removed(el)));
                return unit;
            };
            var diffProp = function(prevEvents, events) {
                return function(v, v1, v11, v2) {
                    if (v11 instanceof Attribute && v2 instanceof Attribute) {
                        var $66 = v11.value2 === v2.value2;
                        if ($66) return v2;
                        setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
                        return v2;
                    }
                    if (v11 instanceof Property && v2 instanceof Property) {
                        var v4 = refEq2(v11.value1, v2.value1);
                        if (v4) return v2;
                        if (v2.value0 === "value") {
                            var elVal = unsafeGetProperty("value", el);
                            var $75 = refEq2(elVal, v2.value1);
                            if ($75) return v2;
                            setProperty(v2.value0, v2.value1, el);
                            return v2;
                        }
                        setProperty(v2.value0, v2.value1, el);
                        return v2;
                    }
                    if (v11 instanceof Handler && v2 instanceof Handler) {
                        var handler3 = unsafeLookup(v2.value0, prevEvents);
                        write(v2.value1)(snd(handler3))();
                        pokeMutMap(v2.value0, handler3, events);
                        return v2;
                    }
                    return v2;
                };
            };
            var applyProp = function(events) {
                return function(v, v1, v2) {
                    if (v2 instanceof Attribute) {
                        setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
                        return v2;
                    }
                    if (v2 instanceof Property) {
                        setProperty(v2.value0, v2.value1, el);
                        return v2;
                    }
                    if (v2 instanceof Handler) {
                        var v3 = unsafeGetAny(v2.value0, events);
                        if (unsafeHasAny(v2.value0, events)) {
                            write(v2.value1)(snd(v3))();
                            return v2;
                        }
                        var ref2 = $$new(v2.value1)();
                        var listener = eventListener(function(ev) {
                            return function __do2() {
                                var f$prime = read(ref2)();
                                return mbEmit(f$prime(ev));
                            };
                        })();
                        pokeMutMap(v2.value0, new Tuple(listener, ref2), events);
                        addEventListener2(v2.value0, listener, el);
                        return v2;
                    }
                    if (v2 instanceof Ref) {
                        mbEmit(v2.value0(new Created(el)));
                        return v2;
                    }
                    throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 113, column 5 - line 135, column 15): " + [
                        v2.constructor.name
                    ]);
                };
            };
            var $lazy_patchProp = $runtime_lazy5("patchProp", "Halogen.VDom.DOM.Prop", function() {
                return function(state3, ps2) {
                    var events = newMutMap();
                    var onThis = removeProp(state3.events);
                    var onThese = diffProp(state3.events, events);
                    var onThat = applyProp(events);
                    var props = diffWithKeyAndIxE(state3.props, ps2, propToStrKey, onThese, onThis, onThat);
                    var nextState = {
                        events: unsafeFreeze2(events),
                        props
                    };
                    return mkStep(new Step(unit, nextState, $lazy_patchProp(100), haltProp));
                };
            });
            var patchProp = $lazy_patchProp(87);
            var renderProp = function(ps1) {
                var events = newMutMap();
                var ps1$prime = strMapWithIxE(ps1, propToStrKey, applyProp(events));
                var state3 = {
                    events: unsafeFreeze2(events),
                    props: ps1$prime
                };
                return mkStep(new Step(unit, state3, patchProp, haltProp));
            };
            return renderProp;
        };
    };
    // output/Halogen.HTML.Core/index.js
    var map11 = /* @__PURE__ */ map(functorArray);
    var map14 = /* @__PURE__ */ map(functorProp);
    var map22 = /* @__PURE__ */ map(functorInput);
    var bimap3 = /* @__PURE__ */ bimap(bifunctorVDom);
    var HTML = function(x) {
        return x;
    };
    var widget = function($28) {
        return HTML(Widget.create($28));
    };
    var text5 = function($29) {
        return HTML(Text.create($29));
    };
    var handler = /* @__PURE__ */ function() {
        return Handler.create;
    }();
    var element = function(ns) {
        return function(name15) {
            return function(props) {
                return function(children2) {
                    return new Elem(ns, name15, props, children2);
                };
            };
        };
    };
    var bifunctorHTML = {
        bimap: function(f) {
            return function(g) {
                return function(v) {
                    return bimap3(map11(map14(map22(g))))(f)(v);
                };
            };
        }
    };
    // output/Control.Applicative.Free/index.js
    var identity6 = /* @__PURE__ */ identity(categoryFn);
    var Pure = /* @__PURE__ */ function() {
        function Pure2(value0) {
            this.value0 = value0;
        }
        Pure2.create = function(value0) {
            return new Pure2(value0);
        };
        return Pure2;
    }();
    var Lift = /* @__PURE__ */ function() {
        function Lift3(value0) {
            this.value0 = value0;
        }
        Lift3.create = function(value0) {
            return new Lift3(value0);
        };
        return Lift3;
    }();
    var Ap = /* @__PURE__ */ function() {
        function Ap2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        Ap2.create = function(value0) {
            return function(value1) {
                return new Ap2(value0, value1);
            };
        };
        return Ap2;
    }();
    var mkAp = function(fba) {
        return function(fb) {
            return new Ap(fba, fb);
        };
    };
    var liftFreeAp = /* @__PURE__ */ function() {
        return Lift.create;
    }();
    var goLeft = function(dictApplicative) {
        var pure13 = pure(dictApplicative);
        return function(fStack) {
            return function(valStack) {
                return function(nat) {
                    return function(func) {
                        return function(count) {
                            if (func instanceof Pure) return new Tuple(new Cons({
                                func: pure13(func.value0),
                                count
                            }, fStack), valStack);
                            if (func instanceof Lift) return new Tuple(new Cons({
                                func: nat(func.value0),
                                count
                            }, fStack), valStack);
                            if (func instanceof Ap) return goLeft(dictApplicative)(fStack)(cons(func.value1)(valStack))(nat)(func.value0)(count + 1 | 0);
                            throw new Error("Failed pattern match at Control.Applicative.Free (line 102, column 41 - line 105, column 81): " + [
                                func.constructor.name
                            ]);
                        };
                    };
                };
            };
        };
    };
    var goApply = function(dictApplicative) {
        var apply2 = apply(dictApplicative.Apply0());
        return function(fStack) {
            return function(vals) {
                return function(gVal) {
                    if (fStack instanceof Nil) return new Left(gVal);
                    if (fStack instanceof Cons) {
                        var gRes = apply2(fStack.value0.func)(gVal);
                        var $31 = fStack.value0.count === 1;
                        if ($31) {
                            if (fStack.value1 instanceof Nil) return new Left(gRes);
                            return goApply(dictApplicative)(fStack.value1)(vals)(gRes);
                        }
                        if (vals instanceof Nil) return new Left(gRes);
                        if (vals instanceof Cons) return new Right(new Tuple(new Cons({
                            func: gRes,
                            count: fStack.value0.count - 1 | 0
                        }, fStack.value1), new NonEmpty(vals.value0, vals.value1)));
                        throw new Error("Failed pattern match at Control.Applicative.Free (line 83, column 11 - line 88, column 50): " + [
                            vals.constructor.name
                        ]);
                    }
                    throw new Error("Failed pattern match at Control.Applicative.Free (line 72, column 3 - line 88, column 50): " + [
                        fStack.constructor.name
                    ]);
                };
            };
        };
    };
    var functorFreeAp = {
        map: function(f) {
            return function(x) {
                return mkAp(new Pure(f))(x);
            };
        }
    };
    var foldFreeAp = function(dictApplicative) {
        var goApply1 = goApply(dictApplicative);
        var pure13 = pure(dictApplicative);
        var goLeft1 = goLeft(dictApplicative);
        return function(nat) {
            return function(z) {
                var go2 = function($copy_v) {
                    var $tco_done = false;
                    var $tco_result;
                    function $tco_loop(v) {
                        if (v.value1.value0 instanceof Pure) {
                            var v1 = goApply1(v.value0)(v.value1.value1)(pure13(v.value1.value0.value0));
                            if (v1 instanceof Left) {
                                $tco_done = true;
                                return v1.value0;
                            }
                            if (v1 instanceof Right) {
                                $copy_v = v1.value0;
                                return;
                            }
                            throw new Error("Failed pattern match at Control.Applicative.Free (line 54, column 17 - line 56, column 24): " + [
                                v1.constructor.name
                            ]);
                        }
                        if (v.value1.value0 instanceof Lift) {
                            var v1 = goApply1(v.value0)(v.value1.value1)(nat(v.value1.value0.value0));
                            if (v1 instanceof Left) {
                                $tco_done = true;
                                return v1.value0;
                            }
                            if (v1 instanceof Right) {
                                $copy_v = v1.value0;
                                return;
                            }
                            throw new Error("Failed pattern match at Control.Applicative.Free (line 57, column 17 - line 59, column 24): " + [
                                v1.constructor.name
                            ]);
                        }
                        if (v.value1.value0 instanceof Ap) {
                            var nextVals = new NonEmpty(v.value1.value0.value1, v.value1.value1);
                            $copy_v = goLeft1(v.value0)(nextVals)(nat)(v.value1.value0.value0)(1);
                            return;
                        }
                        throw new Error("Failed pattern match at Control.Applicative.Free (line 53, column 5 - line 62, column 47): " + [
                            v.value1.value0.constructor.name
                        ]);
                    }
                    while(!$tco_done)$tco_result = $tco_loop($copy_v);
                    return $tco_result;
                };
                return go2(new Tuple(Nil.value, singleton4(z)));
            };
        };
    };
    var retractFreeAp = function(dictApplicative) {
        return foldFreeAp(dictApplicative)(identity6);
    };
    var applyFreeAp = {
        apply: function(fba) {
            return function(fb) {
                return mkAp(fba)(fb);
            };
        },
        Functor0: function() {
            return functorFreeAp;
        }
    };
    var applicativeFreeAp = /* @__PURE__ */ function() {
        return {
            pure: Pure.create,
            Apply0: function() {
                return applyFreeAp;
            }
        };
    }();
    var foldFreeAp1 = /* @__PURE__ */ foldFreeAp(applicativeFreeAp);
    var hoistFreeAp = function(f) {
        return foldFreeAp1(function($54) {
            return liftFreeAp(f($54));
        });
    };
    // output/Data.CatQueue/index.js
    var CatQueue = /* @__PURE__ */ function() {
        function CatQueue2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        CatQueue2.create = function(value0) {
            return function(value1) {
                return new CatQueue2(value0, value1);
            };
        };
        return CatQueue2;
    }();
    var uncons2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
            if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
                $tco_done = true;
                return Nothing.value;
            }
            if (v.value0 instanceof Nil) {
                $copy_v = new CatQueue(reverse(v.value1), Nil.value);
                return;
            }
            if (v.value0 instanceof Cons) {
                $tco_done = true;
                return new Just(new Tuple(v.value0.value0, new CatQueue(v.value0.value1, v.value1)));
            }
            throw new Error("Failed pattern match at Data.CatQueue (line 82, column 1 - line 82, column 63): " + [
                v.constructor.name
            ]);
        }
        while(!$tco_done)$tco_result = $tco_loop($copy_v);
        return $tco_result;
    };
    var snoc2 = function(v) {
        return function(a2) {
            return new CatQueue(v.value0, new Cons(a2, v.value1));
        };
    };
    var $$null2 = function(v) {
        if (v.value0 instanceof Nil && v.value1 instanceof Nil) return true;
        return false;
    };
    var empty5 = /* @__PURE__ */ function() {
        return new CatQueue(Nil.value, Nil.value);
    }();
    // output/Data.CatList/index.js
    var CatNil = /* @__PURE__ */ function() {
        function CatNil2() {}
        CatNil2.value = new CatNil2();
        return CatNil2;
    }();
    var CatCons = /* @__PURE__ */ function() {
        function CatCons2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        CatCons2.create = function(value0) {
            return function(value1) {
                return new CatCons2(value0, value1);
            };
        };
        return CatCons2;
    }();
    var link = function(v) {
        return function(v1) {
            if (v instanceof CatNil) return v1;
            if (v1 instanceof CatNil) return v;
            if (v instanceof CatCons) return new CatCons(v.value0, snoc2(v.value1)(v1));
            throw new Error("Failed pattern match at Data.CatList (line 108, column 1 - line 108, column 54): " + [
                v.constructor.name,
                v1.constructor.name
            ]);
        };
    };
    var foldr3 = function(k) {
        return function(b2) {
            return function(q2) {
                var foldl2 = function($copy_v) {
                    return function($copy_v1) {
                        return function($copy_v2) {
                            var $tco_var_v = $copy_v;
                            var $tco_var_v1 = $copy_v1;
                            var $tco_done = false;
                            var $tco_result;
                            function $tco_loop(v, v1, v2) {
                                if (v2 instanceof Nil) {
                                    $tco_done = true;
                                    return v1;
                                }
                                if (v2 instanceof Cons) {
                                    $tco_var_v = v;
                                    $tco_var_v1 = v(v1)(v2.value0);
                                    $copy_v2 = v2.value1;
                                    return;
                                }
                                throw new Error("Failed pattern match at Data.CatList (line 124, column 3 - line 124, column 59): " + [
                                    v.constructor.name,
                                    v1.constructor.name,
                                    v2.constructor.name
                                ]);
                            }
                            while(!$tco_done)$tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
                            return $tco_result;
                        };
                    };
                };
                var go2 = function($copy_xs) {
                    return function($copy_ys) {
                        var $tco_var_xs = $copy_xs;
                        var $tco_done1 = false;
                        var $tco_result;
                        function $tco_loop(xs, ys) {
                            var v = uncons2(xs);
                            if (v instanceof Nothing) {
                                $tco_done1 = true;
                                return foldl2(function(x) {
                                    return function(i2) {
                                        return i2(x);
                                    };
                                })(b2)(ys);
                            }
                            if (v instanceof Just) {
                                $tco_var_xs = v.value0.value1;
                                $copy_ys = new Cons(k(v.value0.value0), ys);
                                return;
                            }
                            throw new Error("Failed pattern match at Data.CatList (line 120, column 14 - line 122, column 67): " + [
                                v.constructor.name
                            ]);
                        }
                        while(!$tco_done1)$tco_result = $tco_loop($tco_var_xs, $copy_ys);
                        return $tco_result;
                    };
                };
                return go2(q2)(Nil.value);
            };
        };
    };
    var uncons3 = function(v) {
        if (v instanceof CatNil) return Nothing.value;
        if (v instanceof CatCons) return new Just(new Tuple(v.value0, function() {
            var $66 = $$null2(v.value1);
            if ($66) return CatNil.value;
            return foldr3(link)(CatNil.value)(v.value1);
        }()));
        throw new Error("Failed pattern match at Data.CatList (line 99, column 1 - line 99, column 61): " + [
            v.constructor.name
        ]);
    };
    var empty6 = /* @__PURE__ */ function() {
        return CatNil.value;
    }();
    var append2 = link;
    var semigroupCatList = {
        append: append2
    };
    var snoc3 = function(cat) {
        return function(a2) {
            return append2(cat)(new CatCons(a2, empty5));
        };
    };
    // output/Control.Monad.Free/index.js
    var $runtime_lazy6 = function(name15, moduleName, init2) {
        var state3 = 0;
        var val;
        return function(lineNumber) {
            if (state3 === 2) return val;
            if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
            state3 = 1;
            val = init2();
            state3 = 2;
            return val;
        };
    };
    var append3 = /* @__PURE__ */ append(semigroupCatList);
    var map15 = /* @__PURE__ */ map(functorFn);
    var Free = /* @__PURE__ */ function() {
        function Free2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        Free2.create = function(value0) {
            return function(value1) {
                return new Free2(value0, value1);
            };
        };
        return Free2;
    }();
    var Return = /* @__PURE__ */ function() {
        function Return2(value0) {
            this.value0 = value0;
        }
        Return2.create = function(value0) {
            return new Return2(value0);
        };
        return Return2;
    }();
    var Bind = /* @__PURE__ */ function() {
        function Bind2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        Bind2.create = function(value0) {
            return function(value1) {
                return new Bind2(value0, value1);
            };
        };
        return Bind2;
    }();
    var toView = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
            var runExpF = function(v22) {
                return v22;
            };
            var concatF = function(v22) {
                return function(r) {
                    return new Free(v22.value0, append3(v22.value1)(r));
                };
            };
            if (v.value0 instanceof Return) {
                var v2 = uncons3(v.value1);
                if (v2 instanceof Nothing) {
                    $tco_done = true;
                    return new Return(v.value0.value0);
                }
                if (v2 instanceof Just) {
                    $copy_v = concatF(runExpF(v2.value0.value0)(v.value0.value0))(v2.value0.value1);
                    return;
                }
                throw new Error("Failed pattern match at Control.Monad.Free (line 227, column 7 - line 231, column 64): " + [
                    v2.constructor.name
                ]);
            }
            if (v.value0 instanceof Bind) {
                $tco_done = true;
                return new Bind(v.value0.value0, function(a2) {
                    return concatF(v.value0.value1(a2))(v.value1);
                });
            }
            throw new Error("Failed pattern match at Control.Monad.Free (line 225, column 3 - line 233, column 56): " + [
                v.value0.constructor.name
            ]);
        }
        while(!$tco_done)$tco_result = $tco_loop($copy_v);
        return $tco_result;
    };
    var fromView = function(f) {
        return new Free(f, empty6);
    };
    var freeMonad = {
        Applicative0: function() {
            return freeApplicative;
        },
        Bind1: function() {
            return freeBind;
        }
    };
    var freeFunctor = {
        map: function(k) {
            return function(f) {
                return bindFlipped(freeBind)(function() {
                    var $189 = pure(freeApplicative);
                    return function($190) {
                        return $189(k($190));
                    };
                }())(f);
            };
        }
    };
    var freeBind = {
        bind: function(v) {
            return function(k) {
                return new Free(v.value0, snoc3(v.value1)(k));
            };
        },
        Apply0: function() {
            return $lazy_freeApply(0);
        }
    };
    var freeApplicative = {
        pure: function($191) {
            return fromView(Return.create($191));
        },
        Apply0: function() {
            return $lazy_freeApply(0);
        }
    };
    var $lazy_freeApply = /* @__PURE__ */ $runtime_lazy6("freeApply", "Control.Monad.Free", function() {
        return {
            apply: ap(freeMonad),
            Functor0: function() {
                return freeFunctor;
            }
        };
    });
    var bind3 = /* @__PURE__ */ bind(freeBind);
    var pure4 = /* @__PURE__ */ pure(freeApplicative);
    var liftF = function(f) {
        return fromView(new Bind(f, function($192) {
            return pure4($192);
        }));
    };
    var substFree = function(k) {
        var go2 = function(f) {
            var v = toView(f);
            if (v instanceof Return) return pure4(v.value0);
            if (v instanceof Bind) return bind3(k(v.value0))(map15(go2)(v.value1));
            throw new Error("Failed pattern match at Control.Monad.Free (line 168, column 10 - line 170, column 33): " + [
                v.constructor.name
            ]);
        };
        return go2;
    };
    var hoistFree = function(k) {
        return substFree(function($193) {
            return liftF(k($193));
        });
    };
    var foldFree = function(dictMonadRec) {
        var Monad0 = dictMonadRec.Monad0();
        var map110 = map(Monad0.Bind1().Apply0().Functor0());
        var pure13 = pure(Monad0.Applicative0());
        var tailRecM4 = tailRecM(dictMonadRec);
        return function(k) {
            var go2 = function(f) {
                var v = toView(f);
                if (v instanceof Return) return map110(Done.create)(pure13(v.value0));
                if (v instanceof Bind) return map110(function($199) {
                    return Loop.create(v.value1($199));
                })(k(v.value0));
                throw new Error("Failed pattern match at Control.Monad.Free (line 158, column 10 - line 160, column 37): " + [
                    v.constructor.name
                ]);
            };
            return tailRecM4(go2);
        };
    };
    // output/Halogen.Query.ChildQuery/index.js
    var ChildQuery = /* @__PURE__ */ function() {
        function ChildQuery3(value0, value1, value22) {
            this.value0 = value0;
            this.value1 = value1;
            this.value2 = value22;
        }
        ChildQuery3.create = function(value0) {
            return function(value1) {
                return function(value22) {
                    return new ChildQuery3(value0, value1, value22);
                };
            };
        };
        return ChildQuery3;
    }();
    var unChildQueryBox = unsafeCoerce2;
    var mkChildQueryBox = unsafeCoerce2;
    // output/Unsafe.Reference/foreign.js
    function reallyUnsafeRefEq(a2) {
        return function(b2) {
            return a2 === b2;
        };
    }
    // output/Unsafe.Reference/index.js
    var unsafeRefEq = reallyUnsafeRefEq;
    // output/Halogen.Subscription/index.js
    var $$void4 = /* @__PURE__ */ $$void(functorEffect);
    var bind4 = /* @__PURE__ */ bind(bindEffect);
    var append4 = /* @__PURE__ */ append(semigroupArray);
    var traverse_2 = /* @__PURE__ */ traverse_(applicativeEffect);
    var traverse_1 = /* @__PURE__ */ traverse_2(foldableArray);
    var unsubscribe = function(v) {
        return v;
    };
    var subscribe = function(v) {
        return function(k) {
            return v(function($76) {
                return $$void4(k($76));
            });
        };
    };
    var notify = function(v) {
        return function(a2) {
            return v(a2);
        };
    };
    var create3 = function __do() {
        var subscribers = $$new([])();
        return {
            emitter: function(k) {
                return function __do2() {
                    modify_(function(v) {
                        return append4(v)([
                            k
                        ]);
                    })(subscribers)();
                    return modify_(deleteBy(unsafeRefEq)(k))(subscribers);
                };
            },
            listener: function(a2) {
                return bind4(read(subscribers))(traverse_1(function(k) {
                    return k(a2);
                }));
            }
        };
    };
    // output/Halogen.Query.HalogenM/index.js
    var identity7 = /* @__PURE__ */ identity(categoryFn);
    var lookup4 = /* @__PURE__ */ lookup2();
    var over2 = /* @__PURE__ */ over()();
    var SubscriptionId = function(x) {
        return x;
    };
    var ForkId = function(x) {
        return x;
    };
    var State = /* @__PURE__ */ function() {
        function State2(value0) {
            this.value0 = value0;
        }
        State2.create = function(value0) {
            return new State2(value0);
        };
        return State2;
    }();
    var Subscribe = /* @__PURE__ */ function() {
        function Subscribe2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        Subscribe2.create = function(value0) {
            return function(value1) {
                return new Subscribe2(value0, value1);
            };
        };
        return Subscribe2;
    }();
    var Unsubscribe = /* @__PURE__ */ function() {
        function Unsubscribe2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        Unsubscribe2.create = function(value0) {
            return function(value1) {
                return new Unsubscribe2(value0, value1);
            };
        };
        return Unsubscribe2;
    }();
    var Lift2 = /* @__PURE__ */ function() {
        function Lift3(value0) {
            this.value0 = value0;
        }
        Lift3.create = function(value0) {
            return new Lift3(value0);
        };
        return Lift3;
    }();
    var ChildQuery2 = /* @__PURE__ */ function() {
        function ChildQuery3(value0) {
            this.value0 = value0;
        }
        ChildQuery3.create = function(value0) {
            return new ChildQuery3(value0);
        };
        return ChildQuery3;
    }();
    var Raise = /* @__PURE__ */ function() {
        function Raise2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        Raise2.create = function(value0) {
            return function(value1) {
                return new Raise2(value0, value1);
            };
        };
        return Raise2;
    }();
    var Par = /* @__PURE__ */ function() {
        function Par2(value0) {
            this.value0 = value0;
        }
        Par2.create = function(value0) {
            return new Par2(value0);
        };
        return Par2;
    }();
    var Fork = /* @__PURE__ */ function() {
        function Fork2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        Fork2.create = function(value0) {
            return function(value1) {
                return new Fork2(value0, value1);
            };
        };
        return Fork2;
    }();
    var Join = /* @__PURE__ */ function() {
        function Join2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        Join2.create = function(value0) {
            return function(value1) {
                return new Join2(value0, value1);
            };
        };
        return Join2;
    }();
    var Kill = /* @__PURE__ */ function() {
        function Kill2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        Kill2.create = function(value0) {
            return function(value1) {
                return new Kill2(value0, value1);
            };
        };
        return Kill2;
    }();
    var GetRef = /* @__PURE__ */ function() {
        function GetRef2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        GetRef2.create = function(value0) {
            return function(value1) {
                return new GetRef2(value0, value1);
            };
        };
        return GetRef2;
    }();
    var HalogenAp = function(x) {
        return x;
    };
    var HalogenM = function(x) {
        return x;
    };
    var raise = function(o) {
        return liftF(new Raise(o, unit));
    };
    var query = function() {
        return function(dictIsSymbol) {
            var lookup13 = lookup4(dictIsSymbol);
            return function(dictOrd) {
                var lookup23 = lookup13(dictOrd);
                return function(label5) {
                    return function(p2) {
                        return function(q2) {
                            return liftF(new ChildQuery2(mkChildQueryBox(new ChildQuery(function(dictApplicative) {
                                var pure13 = pure(dictApplicative);
                                return function(k) {
                                    var $177 = maybe(pure13(Nothing.value))(k);
                                    var $178 = lookup23(label5)(p2);
                                    return function($179) {
                                        return $177($178($179));
                                    };
                                };
                            }, q2, identity7))));
                        };
                    };
                };
            };
        };
    };
    var ordSubscriptionId = ordInt;
    var ordForkId = ordInt;
    var monadTransHalogenM = {
        lift: function(dictMonad) {
            return function($180) {
                return HalogenM(liftF(Lift2.create($180)));
            };
        }
    };
    var monadHalogenM = freeMonad;
    var monadStateHalogenM = {
        state: function($181) {
            return HalogenM(liftF(State.create($181)));
        },
        Monad0: function() {
            return monadHalogenM;
        }
    };
    var monadEffectHalogenM = function(dictMonadEffect) {
        return {
            liftEffect: function() {
                var $186 = liftEffect(dictMonadEffect);
                return function($187) {
                    return HalogenM(liftF(Lift2.create($186($187))));
                };
            }(),
            Monad0: function() {
                return monadHalogenM;
            }
        };
    };
    var monadAffHalogenM = function(dictMonadAff) {
        var monadEffectHalogenM1 = monadEffectHalogenM(dictMonadAff.MonadEffect0());
        return {
            liftAff: function() {
                var $188 = liftAff(dictMonadAff);
                return function($189) {
                    return HalogenM(liftF(Lift2.create($188($189))));
                };
            }(),
            MonadEffect0: function() {
                return monadEffectHalogenM1;
            }
        };
    };
    var hoist = function(dictFunctor) {
        return function(nat) {
            return function(v) {
                var go2 = function(v1) {
                    if (v1 instanceof State) return new State(v1.value0);
                    if (v1 instanceof Subscribe) return new Subscribe(v1.value0, v1.value1);
                    if (v1 instanceof Unsubscribe) return new Unsubscribe(v1.value0, v1.value1);
                    if (v1 instanceof Lift2) return new Lift2(nat(v1.value0));
                    if (v1 instanceof ChildQuery2) return new ChildQuery2(v1.value0);
                    if (v1 instanceof Raise) return new Raise(v1.value0, v1.value1);
                    if (v1 instanceof Par) return new Par(over2(HalogenAp)(hoistFreeAp(hoist(dictFunctor)(nat)))(v1.value0));
                    if (v1 instanceof Fork) return new Fork(hoist(dictFunctor)(nat)(v1.value0), v1.value1);
                    if (v1 instanceof Join) return new Join(v1.value0, v1.value1);
                    if (v1 instanceof Kill) return new Kill(v1.value0, v1.value1);
                    if (v1 instanceof GetRef) return new GetRef(v1.value0, v1.value1);
                    throw new Error("Failed pattern match at Halogen.Query.HalogenM (line 312, column 8 - line 323, column 29): " + [
                        v1.constructor.name
                    ]);
                };
                return hoistFree(go2)(v);
            };
        };
    };
    var functorHalogenM = freeFunctor;
    var fork2 = function(hmu) {
        return liftF(new Fork(hmu, identity7));
    };
    var bindHalogenM = freeBind;
    var bind5 = /* @__PURE__ */ bind(bindHalogenM);
    var applicativeHalogenM = freeApplicative;
    var pure5 = /* @__PURE__ */ pure(applicativeHalogenM);
    var monadRecHalogenM = {
        tailRecM: function(k) {
            return function(a2) {
                return bind5(k(a2))(function(v) {
                    if (v instanceof Loop) return tailRecM(monadRecHalogenM)(k)(v.value0);
                    if (v instanceof Done) return pure5(v.value0);
                    throw new Error("Failed pattern match at Halogen.Query.HalogenM (line 105, column 26 - line 107, column 21): " + [
                        v.constructor.name
                    ]);
                });
            };
        },
        Monad0: function() {
            return monadHalogenM;
        }
    };
    // output/Halogen.Query.HalogenQ/index.js
    var Initialize = /* @__PURE__ */ function() {
        function Initialize2(value0) {
            this.value0 = value0;
        }
        Initialize2.create = function(value0) {
            return new Initialize2(value0);
        };
        return Initialize2;
    }();
    var Finalize = /* @__PURE__ */ function() {
        function Finalize2(value0) {
            this.value0 = value0;
        }
        Finalize2.create = function(value0) {
            return new Finalize2(value0);
        };
        return Finalize2;
    }();
    var Receive = /* @__PURE__ */ function() {
        function Receive2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        Receive2.create = function(value0) {
            return function(value1) {
                return new Receive2(value0, value1);
            };
        };
        return Receive2;
    }();
    var Action2 = /* @__PURE__ */ function() {
        function Action3(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        Action3.create = function(value0) {
            return function(value1) {
                return new Action3(value0, value1);
            };
        };
        return Action3;
    }();
    var Query = /* @__PURE__ */ function() {
        function Query2(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        }
        Query2.create = function(value0) {
            return function(value1) {
                return new Query2(value0, value1);
            };
        };
        return Query2;
    }();
    // output/Halogen.VDom.Thunk/index.js
    var $runtime_lazy7 = function(name15, moduleName, init2) {
        var state3 = 0;
        var val;
        return function(lineNumber) {
            if (state3 === 2) return val;
            if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
            state3 = 1;
            val = init2();
            state3 = 2;
            return val;
        };
    };
    var Thunk = /* @__PURE__ */ function() {
        function Thunk2(value0, value1, value22, value32) {
            this.value0 = value0;
            this.value1 = value1;
            this.value2 = value22;
            this.value3 = value32;
        }
        Thunk2.create = function(value0) {
            return function(value1) {
                return function(value22) {
                    return function(value32) {
                        return new Thunk2(value0, value1, value22, value32);
                    };
                };
            };
        };
        return Thunk2;
    }();
    var unsafeEqThunk = function(v, v1) {
        return refEq2(v.value0, v1.value0) && refEq2(v.value1, v1.value1) && v.value1(v.value3, v1.value3);
    };
    var runThunk = function(v) {
        return v.value2(v.value3);
    };
    var mapThunk = function(k) {
        return function(v) {
            return new Thunk(v.value0, v.value1, function($51) {
                return k(v.value2($51));
            }, v.value3);
        };
    };
    var hoist2 = mapThunk;
    var buildThunk = function(toVDom) {
        var haltThunk = function(state3) {
            return halt(state3.vdom);
        };
        var $lazy_patchThunk = $runtime_lazy7("patchThunk", "Halogen.VDom.Thunk", function() {
            return function(state3, t2) {
                var $48 = unsafeEqThunk(state3.thunk, t2);
                if ($48) return mkStep(new Step(extract2(state3.vdom), state3, $lazy_patchThunk(112), haltThunk));
                var vdom = step2(state3.vdom, toVDom(runThunk(t2)));
                return mkStep(new Step(extract2(vdom), {
                    vdom,
                    thunk: t2
                }, $lazy_patchThunk(115), haltThunk));
            };
        });
        var patchThunk = $lazy_patchThunk(108);
        var renderThunk = function(spec) {
            return function(t) {
                var vdom = buildVDom(spec)(toVDom(runThunk(t)));
                return mkStep(new Step(extract2(vdom), {
                    thunk: t,
                    vdom
                }, patchThunk, haltThunk));
            };
        };
        return renderThunk;
    };
    // output/Halogen.Component/index.js
    var voidLeft2 = /* @__PURE__ */ voidLeft(functorHalogenM);
    var traverse_3 = /* @__PURE__ */ traverse_(applicativeHalogenM)(foldableMaybe);
    var map16 = /* @__PURE__ */ map(functorHalogenM);
    var lmap2 = /* @__PURE__ */ lmap(bifunctorHTML);
    var pure6 = /* @__PURE__ */ pure(applicativeHalogenM);
    var lookup5 = /* @__PURE__ */ lookup2();
    var pop3 = /* @__PURE__ */ pop2();
    var insert3 = /* @__PURE__ */ insert2();
    var ComponentSlot = /* @__PURE__ */ function() {
        function ComponentSlot2(value0) {
            this.value0 = value0;
        }
        ComponentSlot2.create = function(value0) {
            return new ComponentSlot2(value0);
        };
        return ComponentSlot2;
    }();
    var ThunkSlot = /* @__PURE__ */ function() {
        function ThunkSlot2(value0) {
            this.value0 = value0;
        }
        ThunkSlot2.create = function(value0) {
            return new ThunkSlot2(value0);
        };
        return ThunkSlot2;
    }();
    var unComponentSlot = unsafeCoerce2;
    var unComponent = unsafeCoerce2;
    var mkEval = function(args) {
        return function(v) {
            if (v instanceof Initialize) return voidLeft2(traverse_3(args.handleAction)(args.initialize))(v.value0);
            if (v instanceof Finalize) return voidLeft2(traverse_3(args.handleAction)(args.finalize))(v.value0);
            if (v instanceof Receive) return voidLeft2(traverse_3(args.handleAction)(args.receive(v.value0)))(v.value1);
            if (v instanceof Action2) return voidLeft2(args.handleAction(v.value0))(v.value1);
            if (v instanceof Query) return unCoyoneda(function(g) {
                var $45 = map16(maybe(v.value1(unit))(g));
                return function($46) {
                    return $45(args.handleQuery($46));
                };
            })(v.value0);
            throw new Error("Failed pattern match at Halogen.Component (line 182, column 15 - line 192, column 71): " + [
                v.constructor.name
            ]);
        };
    };
    var mkComponentSlot = unsafeCoerce2;
    var mkComponent = unsafeCoerce2;
    var hoistSlot = function(dictFunctor) {
        return function(nat) {
            return function(v) {
                if (v instanceof ComponentSlot) return unComponentSlot(function(slot3) {
                    return new ComponentSlot(mkComponentSlot({
                        get: slot3.get,
                        pop: slot3.pop,
                        set: slot3.set,
                        component: hoist3(dictFunctor)(nat)(slot3.component),
                        input: slot3.input,
                        output: slot3.output
                    }));
                })(v.value0);
                if (v instanceof ThunkSlot) return new ThunkSlot(hoist2(lmap2(hoistSlot(dictFunctor)(nat)))(v.value0));
                throw new Error("Failed pattern match at Halogen.Component (line 279, column 17 - line 284, column 53): " + [
                    v.constructor.name
                ]);
            };
        };
    };
    var hoist3 = function(dictFunctor) {
        var hoist1 = hoist(dictFunctor);
        return function(nat) {
            return unComponent(function(c) {
                return mkComponent({
                    initialState: c.initialState,
                    render: function() {
                        var $47 = lmap2(hoistSlot(dictFunctor)(nat));
                        return function($48) {
                            return $47(c.render($48));
                        };
                    }(),
                    "eval": function() {
                        var $49 = hoist1(nat);
                        return function($50) {
                            return $49(c["eval"]($50));
                        };
                    }()
                });
            });
        };
    };
    var defaultEval = /* @__PURE__ */ function() {
        return {
            handleAction: $$const(pure6(unit)),
            handleQuery: $$const(pure6(Nothing.value)),
            receive: $$const(Nothing.value),
            initialize: Nothing.value,
            finalize: Nothing.value
        };
    }();
    var componentSlot = function() {
        return function(dictIsSymbol) {
            var lookup13 = lookup5(dictIsSymbol);
            var pop12 = pop3(dictIsSymbol);
            var insert13 = insert3(dictIsSymbol);
            return function(dictOrd) {
                var lookup23 = lookup13(dictOrd);
                var pop22 = pop12(dictOrd);
                var insert22 = insert13(dictOrd);
                return function(label5) {
                    return function(p2) {
                        return function(comp) {
                            return function(input3) {
                                return function(output2) {
                                    return mkComponentSlot({
                                        get: lookup23(label5)(p2),
                                        pop: pop22(label5)(p2),
                                        set: insert22(label5)(p2),
                                        component: comp,
                                        input: input3,
                                        output: output2
                                    });
                                };
                            };
                        };
                    };
                };
            };
        };
    };
    // output/Halogen.HTML.Elements/index.js
    var element2 = /* @__PURE__ */ function() {
        return element(Nothing.value);
    }();
    var div2 = /* @__PURE__ */ element2("div");
    var div_ = /* @__PURE__ */ div2([]);
    var button = /* @__PURE__ */ element2("button");
    // output/Halogen.HTML/index.js
    var componentSlot2 = /* @__PURE__ */ componentSlot();
    var slot = function() {
        return function(dictIsSymbol) {
            var componentSlot1 = componentSlot2(dictIsSymbol);
            return function(dictOrd) {
                var componentSlot22 = componentSlot1(dictOrd);
                return function(label5) {
                    return function(p2) {
                        return function(component5) {
                            return function(input3) {
                                return function(outputQuery) {
                                    return widget(new ComponentSlot(componentSlot22(label5)(p2)(component5)(input3)(function($11) {
                                        return Just.create(outputQuery($11));
                                    })));
                                };
                            };
                        };
                    };
                };
            };
        };
    };
    // output/Halogen.Query/index.js
    var query2 = /* @__PURE__ */ query();
    var identity8 = /* @__PURE__ */ identity(categoryFn);
    var request = function() {
        return function(dictIsSymbol) {
            var query1 = query2(dictIsSymbol);
            return function(dictOrd) {
                var query22 = query1(dictOrd);
                return function(slot3) {
                    return function(label5) {
                        return function(req) {
                            return query22(slot3)(label5)(req(identity8));
                        };
                    };
                };
            };
        };
    };
    // output/Halogen.Aff.Driver.State/index.js
    var unRenderStateX = unsafeCoerce2;
    var unDriverStateX = unsafeCoerce2;
    var renderStateX_ = function(dictApplicative) {
        var traverse_8 = traverse_(dictApplicative)(foldableMaybe);
        return function(f) {
            return unDriverStateX(function(st) {
                return traverse_8(f)(st.rendering);
            });
        };
    };
    var mkRenderStateX = unsafeCoerce2;
    var renderStateX = function(dictFunctor) {
        return function(f) {
            return unDriverStateX(function(st) {
                return mkRenderStateX(f(st.rendering));
            });
        };
    };
    var mkDriverStateXRef = unsafeCoerce2;
    var mapDriverState = function(f) {
        return function(v) {
            return f(v);
        };
    };
    var initDriverState = function(component5) {
        return function(input3) {
            return function(handler3) {
                return function(lchs) {
                    return function __do2() {
                        var selfRef = $$new({})();
                        var childrenIn = $$new(empty3)();
                        var childrenOut = $$new(empty3)();
                        var handlerRef = $$new(handler3)();
                        var pendingQueries = $$new(new Just(Nil.value))();
                        var pendingOuts = $$new(new Just(Nil.value))();
                        var pendingHandlers = $$new(Nothing.value)();
                        var fresh2 = $$new(1)();
                        var subscriptions = $$new(new Just(empty2))();
                        var forks = $$new(empty2)();
                        var ds = {
                            component: component5,
                            state: component5.initialState(input3),
                            refs: empty2,
                            children: empty3,
                            childrenIn,
                            childrenOut,
                            selfRef,
                            handlerRef,
                            pendingQueries,
                            pendingOuts,
                            pendingHandlers,
                            rendering: Nothing.value,
                            fresh: fresh2,
                            subscriptions,
                            forks,
                            lifecycleHandlers: lchs
                        };
                        write(ds)(selfRef)();
                        return mkDriverStateXRef(selfRef);
                    };
                };
            };
        };
    };
    // output/Halogen.Aff.Driver.Eval/index.js
    var traverse_4 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
    var bindFlipped5 = /* @__PURE__ */ bindFlipped(bindMaybe);
    var lookup6 = /* @__PURE__ */ lookup(ordSubscriptionId);
    var bind12 = /* @__PURE__ */ bind(bindAff);
    var liftEffect4 = /* @__PURE__ */ liftEffect(monadEffectAff);
    var discard3 = /* @__PURE__ */ discard(discardUnit);
    var discard1 = /* @__PURE__ */ discard3(bindAff);
    var traverse_12 = /* @__PURE__ */ traverse_(applicativeAff);
    var traverse_22 = /* @__PURE__ */ traverse_12(foldableList);
    var fork3 = /* @__PURE__ */ fork(monadForkAff);
    var parSequence_2 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
    var pure7 = /* @__PURE__ */ pure(applicativeAff);
    var map18 = /* @__PURE__ */ map(functorCoyoneda);
    var parallel3 = /* @__PURE__ */ parallel(parallelAff);
    var map19 = /* @__PURE__ */ map(functorAff);
    var sequential2 = /* @__PURE__ */ sequential(parallelAff);
    var map23 = /* @__PURE__ */ map(functorMaybe);
    var insert4 = /* @__PURE__ */ insert(ordSubscriptionId);
    var retractFreeAp2 = /* @__PURE__ */ retractFreeAp(applicativeParAff);
    var $$delete2 = /* @__PURE__ */ $$delete(ordForkId);
    var unlessM2 = /* @__PURE__ */ unlessM(monadEffect);
    var insert12 = /* @__PURE__ */ insert(ordForkId);
    var traverse_32 = /* @__PURE__ */ traverse_12(foldableMaybe);
    var lookup12 = /* @__PURE__ */ lookup(ordForkId);
    var lookup22 = /* @__PURE__ */ lookup(ordString);
    var foldFree2 = /* @__PURE__ */ foldFree(monadRecAff);
    var alter2 = /* @__PURE__ */ alter(ordString);
    var unsubscribe3 = function(sid) {
        return function(ref2) {
            return function __do2() {
                var v = read(ref2)();
                var subs = read(v.subscriptions)();
                return traverse_4(unsubscribe)(bindFlipped5(lookup6(sid))(subs))();
            };
        };
    };
    var queueOrRun = function(ref2) {
        return function(au) {
            return bind12(liftEffect4(read(ref2)))(function(v) {
                if (v instanceof Nothing) return au;
                if (v instanceof Just) return liftEffect4(write(new Just(new Cons(au, v.value0)))(ref2));
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 188, column 33 - line 190, column 57): " + [
                    v.constructor.name
                ]);
            });
        };
    };
    var handleLifecycle = function(lchs) {
        return function(f) {
            return discard1(liftEffect4(write({
                initializers: Nil.value,
                finalizers: Nil.value
            })(lchs)))(function() {
                return bind12(liftEffect4(f))(function(result) {
                    return bind12(liftEffect4(read(lchs)))(function(v) {
                        return discard1(traverse_22(fork3)(v.finalizers))(function() {
                            return discard1(parSequence_2(v.initializers))(function() {
                                return pure7(result);
                            });
                        });
                    });
                });
            });
        };
    };
    var handleAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure(applicativeEffect)(unit))));
    var fresh = function(f) {
        return function(ref2) {
            return bind12(liftEffect4(read(ref2)))(function(v) {
                return liftEffect4(modify$prime(function(i2) {
                    return {
                        state: i2 + 1 | 0,
                        value: f(i2)
                    };
                })(v.fresh));
            });
        };
    };
    var evalQ = function(render) {
        return function(ref2) {
            return function(q2) {
                return bind12(liftEffect4(read(ref2)))(function(v) {
                    return evalM(render)(ref2)(v["component"]["eval"](new Query(map18(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
                });
            };
        };
    };
    var evalM = function(render) {
        return function(initRef) {
            return function(v) {
                var evalChildQuery = function(ref2) {
                    return function(cqb) {
                        return bind12(liftEffect4(read(ref2)))(function(v1) {
                            return unChildQueryBox(function(v2) {
                                var evalChild = function(v3) {
                                    return parallel3(bind12(liftEffect4(read(v3)))(function(dsx) {
                                        return unDriverStateX(function(ds) {
                                            return evalQ(render)(ds.selfRef)(v2.value1);
                                        })(dsx);
                                    }));
                                };
                                return map19(v2.value2)(sequential2(v2.value0(applicativeParAff)(evalChild)(v1.children)));
                            })(cqb);
                        });
                    };
                };
                var go2 = function(ref2) {
                    return function(v1) {
                        if (v1 instanceof State) return bind12(liftEffect4(read(ref2)))(function(v2) {
                            var v3 = v1.value0(v2.state);
                            if (unsafeRefEq(v2.state)(v3.value1)) return pure7(v3.value0);
                            if (otherwise) return discard1(liftEffect4(write({
                                component: v2.component,
                                state: v3.value1,
                                refs: v2.refs,
                                children: v2.children,
                                childrenIn: v2.childrenIn,
                                childrenOut: v2.childrenOut,
                                selfRef: v2.selfRef,
                                handlerRef: v2.handlerRef,
                                pendingQueries: v2.pendingQueries,
                                pendingOuts: v2.pendingOuts,
                                pendingHandlers: v2.pendingHandlers,
                                rendering: v2.rendering,
                                fresh: v2.fresh,
                                subscriptions: v2.subscriptions,
                                forks: v2.forks,
                                lifecycleHandlers: v2.lifecycleHandlers
                            })(ref2)))(function() {
                                return discard1(handleLifecycle(v2.lifecycleHandlers)(render(v2.lifecycleHandlers)(ref2)))(function() {
                                    return pure7(v3.value0);
                                });
                            });
                            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 86, column 7 - line 92, column 21): " + [
                                v3.constructor.name
                            ]);
                        });
                        if (v1 instanceof Subscribe) return bind12(fresh(SubscriptionId)(ref2))(function(sid) {
                            return bind12(liftEffect4(subscribe(v1.value0(sid))(function(act) {
                                return handleAff(evalF(render)(ref2)(new Action(act)));
                            })))(function(finalize) {
                                return bind12(liftEffect4(read(ref2)))(function(v2) {
                                    return discard1(liftEffect4(modify_(map23(insert4(sid)(finalize)))(v2.subscriptions)))(function() {
                                        return pure7(v1.value1(sid));
                                    });
                                });
                            });
                        });
                        if (v1 instanceof Unsubscribe) return discard1(liftEffect4(unsubscribe3(v1.value0)(ref2)))(function() {
                            return pure7(v1.value1);
                        });
                        if (v1 instanceof Lift2) return v1.value0;
                        if (v1 instanceof ChildQuery2) return evalChildQuery(ref2)(v1.value0);
                        if (v1 instanceof Raise) return bind12(liftEffect4(read(ref2)))(function(v2) {
                            return bind12(liftEffect4(read(v2.handlerRef)))(function(handler3) {
                                return discard1(queueOrRun(v2.pendingOuts)(handler3(v1.value0)))(function() {
                                    return pure7(v1.value1);
                                });
                            });
                        });
                        if (v1 instanceof Par) return sequential2(retractFreeAp2(hoistFreeAp(function() {
                            var $119 = evalM(render)(ref2);
                            return function($120) {
                                return parallel3($119($120));
                            };
                        }())(v1.value0)));
                        if (v1 instanceof Fork) return bind12(fresh(ForkId)(ref2))(function(fid) {
                            return bind12(liftEffect4(read(ref2)))(function(v2) {
                                return bind12(liftEffect4($$new(false)))(function(doneRef) {
                                    return bind12(fork3($$finally(liftEffect4(function __do2() {
                                        modify_($$delete2(fid))(v2.forks)();
                                        return write(true)(doneRef)();
                                    }))(evalM(render)(ref2)(v1.value0))))(function(fiber) {
                                        return discard1(liftEffect4(unlessM2(read(doneRef))(modify_(insert12(fid)(fiber))(v2.forks))))(function() {
                                            return pure7(v1.value1(fid));
                                        });
                                    });
                                });
                            });
                        });
                        if (v1 instanceof Join) return bind12(liftEffect4(read(ref2)))(function(v2) {
                            return bind12(liftEffect4(read(v2.forks)))(function(forkMap) {
                                return discard1(traverse_32(joinFiber)(lookup12(v1.value0)(forkMap)))(function() {
                                    return pure7(v1.value1);
                                });
                            });
                        });
                        if (v1 instanceof Kill) return bind12(liftEffect4(read(ref2)))(function(v2) {
                            return bind12(liftEffect4(read(v2.forks)))(function(forkMap) {
                                return discard1(traverse_32(killFiber(error("Cancelled")))(lookup12(v1.value0)(forkMap)))(function() {
                                    return pure7(v1.value1);
                                });
                            });
                        });
                        if (v1 instanceof GetRef) return bind12(liftEffect4(read(ref2)))(function(v2) {
                            return pure7(v1.value1(lookup22(v1.value0)(v2.refs)));
                        });
                        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 83, column 12 - line 139, column 33): " + [
                            v1.constructor.name
                        ]);
                    };
                };
                return foldFree2(go2(initRef))(v);
            };
        };
    };
    var evalF = function(render) {
        return function(ref2) {
            return function(v) {
                if (v instanceof RefUpdate) return liftEffect4(flip(modify_)(ref2)(mapDriverState(function(st) {
                    return {
                        component: st.component,
                        state: st.state,
                        refs: alter2($$const(v.value1))(v.value0)(st.refs),
                        children: st.children,
                        childrenIn: st.childrenIn,
                        childrenOut: st.childrenOut,
                        selfRef: st.selfRef,
                        handlerRef: st.handlerRef,
                        pendingQueries: st.pendingQueries,
                        pendingOuts: st.pendingOuts,
                        pendingHandlers: st.pendingHandlers,
                        rendering: st.rendering,
                        fresh: st.fresh,
                        subscriptions: st.subscriptions,
                        forks: st.forks,
                        lifecycleHandlers: st.lifecycleHandlers
                    };
                })));
                if (v instanceof Action) return bind12(liftEffect4(read(ref2)))(function(v1) {
                    return evalM(render)(ref2)(v1["component"]["eval"](new Action2(v.value0, unit)));
                });
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 52, column 20 - line 58, column 62): " + [
                    v.constructor.name
                ]);
            };
        };
    };
    // output/Halogen.Aff.Driver/index.js
    var bind6 = /* @__PURE__ */ bind(bindEffect);
    var discard4 = /* @__PURE__ */ discard(discardUnit);
    var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
    var traverse_5 = /* @__PURE__ */ traverse_(applicativeAff)(foldableList);
    var fork4 = /* @__PURE__ */ fork(monadForkAff);
    var bindFlipped6 = /* @__PURE__ */ bindFlipped(bindEffect);
    var traverse_13 = /* @__PURE__ */ traverse_(applicativeEffect);
    var traverse_23 = /* @__PURE__ */ traverse_13(foldableMaybe);
    var traverse_33 = /* @__PURE__ */ traverse_13(foldableMap);
    var discard22 = /* @__PURE__ */ discard4(bindAff);
    var parSequence_3 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
    var liftEffect5 = /* @__PURE__ */ liftEffect(monadEffectAff);
    var pure8 = /* @__PURE__ */ pure(applicativeEffect);
    var map20 = /* @__PURE__ */ map(functorEffect);
    var pure12 = /* @__PURE__ */ pure(applicativeAff);
    var when2 = /* @__PURE__ */ when(applicativeEffect);
    var renderStateX2 = /* @__PURE__ */ renderStateX(functorEffect);
    var $$void5 = /* @__PURE__ */ $$void(functorAff);
    var foreachSlot2 = /* @__PURE__ */ foreachSlot(applicativeEffect);
    var renderStateX_2 = /* @__PURE__ */ renderStateX_(applicativeEffect);
    var tailRecM3 = /* @__PURE__ */ tailRecM(monadRecEffect);
    var voidLeft3 = /* @__PURE__ */ voidLeft(functorEffect);
    var bind13 = /* @__PURE__ */ bind(bindAff);
    var liftEffect1 = /* @__PURE__ */ liftEffect(monadEffectEffect);
    var newLifecycleHandlers = /* @__PURE__ */ function() {
        return $$new({
            initializers: Nil.value,
            finalizers: Nil.value
        });
    }();
    var handlePending = function(ref2) {
        return function __do2() {
            var queue = read(ref2)();
            write(Nothing.value)(ref2)();
            return for_2(queue)(function() {
                var $59 = traverse_5(fork4);
                return function($60) {
                    return handleAff($59(reverse($60)));
                };
            }())();
        };
    };
    var cleanupSubscriptionsAndForks = function(v) {
        return function __do2() {
            bindFlipped6(traverse_23(traverse_33(unsubscribe)))(read(v.subscriptions))();
            write(Nothing.value)(v.subscriptions)();
            bindFlipped6(traverse_33(function() {
                var $61 = killFiber(error("finalized"));
                return function($62) {
                    return handleAff($61($62));
                };
            }()))(read(v.forks))();
            return write(empty2)(v.forks)();
        };
    };
    var runUI = function(renderSpec2) {
        return function(component5) {
            return function(i2) {
                var squashChildInitializers = function(lchs) {
                    return function(preInits) {
                        return unDriverStateX(function(st) {
                            var parentInitializer = evalM(render)(st.selfRef)(st["component"]["eval"](new Initialize(unit)));
                            return modify_(function(handlers) {
                                return {
                                    initializers: new Cons(discard22(parSequence_3(reverse(handlers.initializers)))(function() {
                                        return discard22(parentInitializer)(function() {
                                            return liftEffect5(function __do2() {
                                                handlePending(st.pendingQueries)();
                                                return handlePending(st.pendingOuts)();
                                            });
                                        });
                                    }), preInits),
                                    finalizers: handlers.finalizers
                                };
                            })(lchs);
                        });
                    };
                };
                var runComponent = function(lchs) {
                    return function(handler3) {
                        return function(j) {
                            return unComponent(function(c) {
                                return function __do2() {
                                    var lchs$prime = newLifecycleHandlers();
                                    var $$var2 = initDriverState(c)(j)(handler3)(lchs$prime)();
                                    var pre2 = read(lchs)();
                                    write({
                                        initializers: Nil.value,
                                        finalizers: pre2.finalizers
                                    })(lchs)();
                                    bindFlipped6(unDriverStateX(function() {
                                        var $63 = render(lchs);
                                        return function($64) {
                                            return $63(function(v) {
                                                return v.selfRef;
                                            }($64));
                                        };
                                    }()))(read($$var2))();
                                    bindFlipped6(squashChildInitializers(lchs)(pre2.initializers))(read($$var2))();
                                    return $$var2;
                                };
                            });
                        };
                    };
                };
                var renderChild = function(lchs) {
                    return function(handler3) {
                        return function(childrenInRef) {
                            return function(childrenOutRef) {
                                return unComponentSlot(function(slot3) {
                                    return function __do2() {
                                        var childrenIn = map20(slot3.pop)(read(childrenInRef))();
                                        var $$var2 = function() {
                                            if (childrenIn instanceof Just) {
                                                write(childrenIn.value0.value1)(childrenInRef)();
                                                var dsx = read(childrenIn.value0.value0)();
                                                unDriverStateX(function(st) {
                                                    return function __do3() {
                                                        flip(write)(st.handlerRef)(function() {
                                                            var $65 = maybe(pure12(unit))(handler3);
                                                            return function($66) {
                                                                return $65(slot3.output($66));
                                                            };
                                                        }())();
                                                        return handleAff(evalM(render)(st.selfRef)(st["component"]["eval"](new Receive(slot3.input, unit))))();
                                                    };
                                                })(dsx)();
                                                return childrenIn.value0.value0;
                                            }
                                            if (childrenIn instanceof Nothing) return runComponent(lchs)(function() {
                                                var $67 = maybe(pure12(unit))(handler3);
                                                return function($68) {
                                                    return $67(slot3.output($68));
                                                };
                                            }())(slot3.input)(slot3.component)();
                                            throw new Error("Failed pattern match at Halogen.Aff.Driver (line 213, column 14 - line 222, column 98): " + [
                                                childrenIn.constructor.name
                                            ]);
                                        }();
                                        var isDuplicate = map20(function($69) {
                                            return isJust(slot3.get($69));
                                        })(read(childrenOutRef))();
                                        when2(isDuplicate)(warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                                        modify_(slot3.set($$var2))(childrenOutRef)();
                                        return bind6(read($$var2))(renderStateX2(function(v) {
                                            if (v instanceof Nothing) return $$throw("Halogen internal error: child was not initialized in renderChild");
                                            if (v instanceof Just) return pure8(renderSpec2.renderChild(v.value0));
                                            throw new Error("Failed pattern match at Halogen.Aff.Driver (line 227, column 37 - line 229, column 50): " + [
                                                v.constructor.name
                                            ]);
                                        }))();
                                    };
                                });
                            };
                        };
                    };
                };
                var render = function(lchs) {
                    return function($$var2) {
                        return function __do2() {
                            var v = read($$var2)();
                            var shouldProcessHandlers = map20(isNothing)(read(v.pendingHandlers))();
                            when2(shouldProcessHandlers)(write(new Just(Nil.value))(v.pendingHandlers))();
                            write(empty3)(v.childrenOut)();
                            write(v.children)(v.childrenIn)();
                            var handler3 = function() {
                                var $70 = queueOrRun(v.pendingHandlers);
                                var $71 = evalF(render)(v.selfRef);
                                return function($72) {
                                    return $70($$void5($71($72)));
                                };
                            }();
                            var childHandler = function() {
                                var $73 = queueOrRun(v.pendingQueries);
                                return function($74) {
                                    return $73(handler3(Action.create($74)));
                                };
                            }();
                            var rendering = renderSpec2.render(function($75) {
                                return handleAff(handler3($75));
                            })(renderChild(lchs)(childHandler)(v.childrenIn)(v.childrenOut))(v.component.render(v.state))(v.rendering)();
                            var children2 = read(v.childrenOut)();
                            var childrenIn = read(v.childrenIn)();
                            foreachSlot2(childrenIn)(function(v1) {
                                return function __do3() {
                                    var childDS = read(v1)();
                                    renderStateX_2(renderSpec2.removeChild)(childDS)();
                                    return finalize(lchs)(childDS)();
                                };
                            })();
                            flip(modify_)(v.selfRef)(mapDriverState(function(ds$prime) {
                                return {
                                    component: ds$prime.component,
                                    state: ds$prime.state,
                                    refs: ds$prime.refs,
                                    children: children2,
                                    childrenIn: ds$prime.childrenIn,
                                    childrenOut: ds$prime.childrenOut,
                                    selfRef: ds$prime.selfRef,
                                    handlerRef: ds$prime.handlerRef,
                                    pendingQueries: ds$prime.pendingQueries,
                                    pendingOuts: ds$prime.pendingOuts,
                                    pendingHandlers: ds$prime.pendingHandlers,
                                    rendering: new Just(rendering),
                                    fresh: ds$prime.fresh,
                                    subscriptions: ds$prime.subscriptions,
                                    forks: ds$prime.forks,
                                    lifecycleHandlers: ds$prime.lifecycleHandlers
                                };
                            }))();
                            return when2(shouldProcessHandlers)(flip(tailRecM3)(unit)(function(v1) {
                                return function __do3() {
                                    var handlers = read(v.pendingHandlers)();
                                    write(new Just(Nil.value))(v.pendingHandlers)();
                                    traverse_23(function() {
                                        var $76 = traverse_5(fork4);
                                        return function($77) {
                                            return handleAff($76(reverse($77)));
                                        };
                                    }())(handlers)();
                                    var mmore = read(v.pendingHandlers)();
                                    var $52 = maybe(false)($$null)(mmore);
                                    if ($52) return voidLeft3(write(Nothing.value)(v.pendingHandlers))(new Done(unit))();
                                    return new Loop(unit);
                                };
                            }))();
                        };
                    };
                };
                var finalize = function(lchs) {
                    return unDriverStateX(function(st) {
                        return function __do2() {
                            cleanupSubscriptionsAndForks(st)();
                            var f = evalM(render)(st.selfRef)(st["component"]["eval"](new Finalize(unit)));
                            modify_(function(handlers) {
                                return {
                                    initializers: handlers.initializers,
                                    finalizers: new Cons(f, handlers.finalizers)
                                };
                            })(lchs)();
                            return foreachSlot2(st.children)(function(v) {
                                return function __do3() {
                                    var dsx = read(v)();
                                    return finalize(lchs)(dsx)();
                                };
                            })();
                        };
                    });
                };
                var evalDriver = function(disposed) {
                    return function(ref2) {
                        return function(q2) {
                            return bind13(liftEffect5(read(disposed)))(function(v) {
                                if (v) return pure12(Nothing.value);
                                return evalQ(render)(ref2)(q2);
                            });
                        };
                    };
                };
                var dispose = function(disposed) {
                    return function(lchs) {
                        return function(dsx) {
                            return handleLifecycle(lchs)(function __do2() {
                                var v = read(disposed)();
                                if (v) return unit;
                                write(true)(disposed)();
                                finalize(lchs)(dsx)();
                                return unDriverStateX(function(v1) {
                                    return function __do3() {
                                        var v2 = liftEffect1(read(v1.selfRef))();
                                        return for_2(v2.rendering)(renderSpec2.dispose)();
                                    };
                                })(dsx)();
                            });
                        };
                    };
                };
                return bind13(liftEffect5(newLifecycleHandlers))(function(lchs) {
                    return bind13(liftEffect5($$new(false)))(function(disposed) {
                        return handleLifecycle(lchs)(function __do2() {
                            var sio = create3();
                            var dsx = bindFlipped6(read)(runComponent(lchs)(function() {
                                var $78 = notify(sio.listener);
                                return function($79) {
                                    return liftEffect5($78($79));
                                };
                            }())(i2)(component5))();
                            return unDriverStateX(function(st) {
                                return pure8({
                                    query: evalDriver(disposed)(st.selfRef),
                                    messages: sio.emitter,
                                    dispose: dispose(disposed)(lchs)(dsx)
                                });
                            })(dsx)();
                        });
                    });
                });
            };
        };
    };
    // output/Web.DOM.Node/foreign.js
    var getEffProp2 = function(name15) {
        return function(node) {
            return function() {
                return node[name15];
            };
        };
    };
    var baseURI = getEffProp2("baseURI");
    var _ownerDocument = getEffProp2("ownerDocument");
    var _parentNode = getEffProp2("parentNode");
    var _parentElement = getEffProp2("parentElement");
    var childNodes = getEffProp2("childNodes");
    var _firstChild = getEffProp2("firstChild");
    var _lastChild = getEffProp2("lastChild");
    var _previousSibling = getEffProp2("previousSibling");
    var _nextSibling = getEffProp2("nextSibling");
    var _nodeValue = getEffProp2("nodeValue");
    var textContent = getEffProp2("textContent");
    function insertBefore(node1) {
        return function(node2) {
            return function(parent2) {
                return function() {
                    parent2.insertBefore(node1, node2);
                };
            };
        };
    }
    function appendChild(node) {
        return function(parent2) {
            return function() {
                parent2.appendChild(node);
            };
        };
    }
    function removeChild2(node) {
        return function(parent2) {
            return function() {
                parent2.removeChild(node);
            };
        };
    }
    // output/Web.DOM.Node/index.js
    var map21 = /* @__PURE__ */ map(functorEffect);
    var parentNode2 = /* @__PURE__ */ function() {
        var $6 = map21(toMaybe);
        return function($7) {
            return $6(_parentNode($7));
        };
    }();
    var nextSibling = /* @__PURE__ */ function() {
        var $15 = map21(toMaybe);
        return function($16) {
            return $15(_nextSibling($16));
        };
    }();
    // output/Halogen.VDom.Driver/index.js
    var $runtime_lazy8 = function(name15, moduleName, init2) {
        var state3 = 0;
        var val;
        return function(lineNumber) {
            if (state3 === 2) return val;
            if (state3 === 1) throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
            state3 = 1;
            val = init2();
            state3 = 2;
            return val;
        };
    };
    var $$void6 = /* @__PURE__ */ $$void(functorEffect);
    var pure9 = /* @__PURE__ */ pure(applicativeEffect);
    var traverse_6 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
    var unwrap2 = /* @__PURE__ */ unwrap();
    var when3 = /* @__PURE__ */ when(applicativeEffect);
    var not2 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean)));
    var identity9 = /* @__PURE__ */ identity(categoryFn);
    var bind14 = /* @__PURE__ */ bind(bindAff);
    var liftEffect6 = /* @__PURE__ */ liftEffect(monadEffectAff);
    var map24 = /* @__PURE__ */ map(functorEffect);
    var bindFlipped7 = /* @__PURE__ */ bindFlipped(bindEffect);
    var substInParent = function(v) {
        return function(v1) {
            return function(v2) {
                if (v1 instanceof Just && v2 instanceof Just) return $$void6(insertBefore(v)(v1.value0)(v2.value0));
                if (v1 instanceof Nothing && v2 instanceof Just) return $$void6(appendChild(v)(v2.value0));
                return pure9(unit);
            };
        };
    };
    var removeChild3 = function(v) {
        return function __do2() {
            var npn = parentNode2(v.node)();
            return traverse_6(function(pn) {
                return removeChild2(v.node)(pn);
            })(npn)();
        };
    };
    var mkSpec = function(handler3) {
        return function(renderChildRef) {
            return function(document2) {
                var getNode = unRenderStateX(function(v) {
                    return v.node;
                });
                var done = function(st) {
                    if (st instanceof Just) return halt(st.value0);
                    return unit;
                };
                var buildWidget2 = function(spec) {
                    var buildThunk2 = buildThunk(unwrap2)(spec);
                    var $lazy_patch = $runtime_lazy8("patch", "Halogen.VDom.Driver", function() {
                        return function(st, slot3) {
                            if (st instanceof Just) {
                                if (slot3 instanceof ComponentSlot) {
                                    halt(st.value0);
                                    return $lazy_renderComponentSlot(100)(slot3.value0);
                                }
                                if (slot3 instanceof ThunkSlot) {
                                    var step$prime = step2(st.value0, slot3.value0);
                                    return mkStep(new Step(extract2(step$prime), new Just(step$prime), $lazy_patch(103), done));
                                }
                                throw new Error("Failed pattern match at Halogen.VDom.Driver (line 97, column 22 - line 103, column 79): " + [
                                    slot3.constructor.name
                                ]);
                            }
                            return $lazy_render(104)(slot3);
                        };
                    });
                    var $lazy_render = $runtime_lazy8("render", "Halogen.VDom.Driver", function() {
                        return function(slot3) {
                            if (slot3 instanceof ComponentSlot) return $lazy_renderComponentSlot(86)(slot3.value0);
                            if (slot3 instanceof ThunkSlot) {
                                var step3 = buildThunk2(slot3.value0);
                                return mkStep(new Step(extract2(step3), new Just(step3), $lazy_patch(89), done));
                            }
                            throw new Error("Failed pattern match at Halogen.VDom.Driver (line 84, column 7 - line 89, column 75): " + [
                                slot3.constructor.name
                            ]);
                        };
                    });
                    var $lazy_renderComponentSlot = $runtime_lazy8("renderComponentSlot", "Halogen.VDom.Driver", function() {
                        return function(cs) {
                            var renderChild = read(renderChildRef)();
                            var rsx = renderChild(cs)();
                            var node = getNode(rsx);
                            return mkStep(new Step(node, Nothing.value, $lazy_patch(117), done));
                        };
                    });
                    var patch = $lazy_patch(91);
                    var render = $lazy_render(82);
                    var renderComponentSlot = $lazy_renderComponentSlot(109);
                    return render;
                };
                var buildAttributes = buildProp(handler3);
                return {
                    buildWidget: buildWidget2,
                    buildAttributes,
                    document: document2
                };
            };
        };
    };
    var renderSpec = function(document2) {
        return function(container) {
            var render = function(handler3) {
                return function(child) {
                    return function(v) {
                        return function(v1) {
                            if (v1 instanceof Nothing) return function __do2() {
                                var renderChildRef = $$new(child)();
                                var spec = mkSpec(handler3)(renderChildRef)(document2);
                                var machine = buildVDom(spec)(v);
                                var node = extract2(machine);
                                $$void6(appendChild(node)(toNode(container)))();
                                return {
                                    machine,
                                    node,
                                    renderChildRef
                                };
                            };
                            if (v1 instanceof Just) return function __do2() {
                                write(child)(v1.value0.renderChildRef)();
                                var parent2 = parentNode2(v1.value0.node)();
                                var nextSib = nextSibling(v1.value0.node)();
                                var machine$prime = step2(v1.value0.machine, v);
                                var newNode = extract2(machine$prime);
                                when3(not2(unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(nextSib)(parent2))();
                                return {
                                    machine: machine$prime,
                                    node: newNode,
                                    renderChildRef: v1.value0.renderChildRef
                                };
                            };
                            throw new Error("Failed pattern match at Halogen.VDom.Driver (line 157, column 5 - line 173, column 80): " + [
                                v1.constructor.name
                            ]);
                        };
                    };
                };
            };
            return {
                render,
                renderChild: identity9,
                removeChild: removeChild3,
                dispose: removeChild3
            };
        };
    };
    var runUI2 = function(component5) {
        return function(i2) {
            return function(element3) {
                return bind14(liftEffect6(map24(toDocument)(bindFlipped7(document)(windowImpl))))(function(document2) {
                    return runUI(renderSpec(document2)(element3))(component5)(i2);
                });
            };
        };
    };
    // output/Web.UIEvent.MouseEvent.EventTypes/index.js
    var click2 = "click";
    // output/Halogen.HTML.Events/index.js
    var mouseHandler = unsafeCoerce2;
    var handler2 = function(et) {
        return function(f) {
            return handler(et)(function(ev) {
                return new Just(new Action(f(ev)));
            });
        };
    };
    var onClick = /* @__PURE__ */ function() {
        var $15 = handler2(click2);
        return function($16) {
            return $15(mouseHandler($16));
        };
    }();
    // output/Child/index.js
    var bind7 = /* @__PURE__ */ bind(bindHalogenM);
    var get2 = /* @__PURE__ */ get(monadStateHalogenM);
    var pure10 = /* @__PURE__ */ pure(applicativeHalogenM);
    var discard5 = /* @__PURE__ */ discard(discardUnit)(bindHalogenM);
    var modify_3 = /* @__PURE__ */ modify_2(monadStateHalogenM);
    var GetCount = /* @__PURE__ */ function() {
        function GetCount2(value0) {
            this.value0 = value0;
        }
        GetCount2.create = function(value0) {
            return new GetCount2(value0);
        };
        return GetCount2;
    }();
    var Clicked = /* @__PURE__ */ function() {
        function Clicked2() {}
        Clicked2.value = new Clicked2();
        return Clicked2;
    }();
    var HandleClick = /* @__PURE__ */ function() {
        function HandleClick2() {}
        HandleClick2.value = new HandleClick2();
        return HandleClick2;
    }();
    var component = /* @__PURE__ */ function() {
        var handleQuery = function(v) {
            return bind7(get2)(function($$int) {
                return pure10(new Just(v.value0($$int)));
            });
        };
        var handleAction = function(v) {
            return discard5(modify_3(function(v1) {
                return v1 + 1 | 0;
            }))(function() {
                return raise(Clicked.value);
            });
        };
        return mkComponent({
            initialState: $$const(0),
            render: function(v) {
                return div_([
                    button([
                        onClick(function(v1) {
                            return HandleClick.value;
                        })
                    ])([
                        text5("I'm the child.")
                    ]),
                    text5("I'm rendered within the parent in the component tree, but elsewhere in the DOM.")
                ]);
            },
            "eval": mkEval({
                handleAction,
                handleQuery,
                receive: defaultEval.receive,
                initialize: defaultEval.initialize,
                finalize: defaultEval.finalize
            })
        });
    }();
    // output/Effect.Class.Console/index.js
    var logShow2 = function(dictMonadEffect) {
        var liftEffect8 = liftEffect(dictMonadEffect);
        return function(dictShow) {
            var $49 = logShow(dictShow);
            return function($50) {
                return liftEffect8($49($50));
            };
        };
    };
    var log3 = function(dictMonadEffect) {
        var $51 = liftEffect(dictMonadEffect);
        return function($52) {
            return $51(log($52));
        };
    };
    // output/Effect.AVar/foreign.js
    var AVar = function() {
        function MutableQueue() {
            this.head = null;
            this.last = null;
            this.size = 0;
        }
        function MutableCell(queue, value12) {
            this.queue = queue;
            this.value = value12;
            this.next = null;
            this.prev = null;
        }
        function AVar2(value12) {
            this.draining = false;
            this.error = null;
            this.value = value12;
            this.takes = new MutableQueue();
            this.reads = new MutableQueue();
            this.puts = new MutableQueue();
        }
        var EMPTY = {};
        function runEff(eff) {
            try {
                eff();
            } catch (error4) {
                setTimeout(function() {
                    throw error4;
                }, 0);
            }
        }
        function putLast(queue, value12) {
            var cell = new MutableCell(queue, value12);
            switch(queue.size){
                case 0:
                    queue.head = cell;
                    break;
                case 1:
                    cell.prev = queue.head;
                    queue.head.next = cell;
                    queue.last = cell;
                    break;
                default:
                    cell.prev = queue.last;
                    queue.last.next = cell;
                    queue.last = cell;
            }
            queue.size++;
            return cell;
        }
        function takeLast(queue) {
            var cell;
            switch(queue.size){
                case 0:
                    return null;
                case 1:
                    cell = queue.head;
                    queue.head = null;
                    break;
                case 2:
                    cell = queue.last;
                    queue.head.next = null;
                    queue.last = null;
                    break;
                default:
                    cell = queue.last;
                    queue.last = cell.prev;
                    queue.last.next = null;
            }
            cell.prev = null;
            cell.queue = null;
            queue.size--;
            return cell.value;
        }
        function takeHead(queue) {
            var cell;
            switch(queue.size){
                case 0:
                    return null;
                case 1:
                    cell = queue.head;
                    queue.head = null;
                    break;
                case 2:
                    cell = queue.head;
                    queue.last.prev = null;
                    queue.head = queue.last;
                    queue.last = null;
                    break;
                default:
                    cell = queue.head;
                    queue.head = cell.next;
                    queue.head.prev = null;
            }
            cell.next = null;
            cell.queue = null;
            queue.size--;
            return cell.value;
        }
        function deleteCell2(cell) {
            if (cell.queue === null) return;
            if (cell.queue.last === cell) {
                takeLast(cell.queue);
                return;
            }
            if (cell.queue.head === cell) {
                takeHead(cell.queue);
                return;
            }
            if (cell.prev) cell.prev.next = cell.next;
            if (cell.next) cell.next.prev = cell.prev;
            cell.queue.size--;
            cell.queue = null;
            cell.value = null;
            cell.next = null;
            cell.prev = null;
        }
        function drainVar(util, avar) {
            if (avar.draining) return;
            var ps = avar.puts;
            var ts = avar.takes;
            var rs = avar.reads;
            var p2, r, t, value12, rsize;
            avar.draining = true;
            while(true){
                p2 = null;
                r = null;
                t = null;
                value12 = avar.value;
                rsize = rs.size;
                if (avar.error !== null) {
                    value12 = util.left(avar.error);
                    while(p2 = takeHead(ps))runEff(p2.cb(value12));
                    while(r = takeHead(rs))runEff(r(value12));
                    while(t = takeHead(ts))runEff(t(value12));
                    break;
                }
                if (value12 === EMPTY && (p2 = takeHead(ps))) avar.value = value12 = p2.value;
                if (value12 !== EMPTY) {
                    t = takeHead(ts);
                    while(rsize-- && (r = takeHead(rs)))runEff(r(util.right(value12)));
                    if (t !== null) {
                        avar.value = EMPTY;
                        runEff(t(util.right(value12)));
                    }
                }
                if (p2 !== null) runEff(p2.cb(util.right(void 0)));
                if (avar.value === EMPTY && ps.size === 0 || avar.value !== EMPTY && ts.size === 0) break;
            }
            avar.draining = false;
        }
        AVar2.EMPTY = EMPTY;
        AVar2.putLast = putLast;
        AVar2.takeLast = takeLast;
        AVar2.takeHead = takeHead;
        AVar2.deleteCell = deleteCell2;
        AVar2.drainVar = drainVar;
        return AVar2;
    }();
    function empty7() {
        return new AVar(AVar.EMPTY);
    }
    function _putVar(util, value12, avar, cb) {
        return function() {
            var cell = AVar.putLast(avar.puts, {
                cb,
                value: value12
            });
            AVar.drainVar(util, avar);
            return function() {
                AVar.deleteCell(cell);
            };
        };
    }
    function _takeVar(util, avar, cb) {
        return function() {
            var cell = AVar.putLast(avar.takes, cb);
            AVar.drainVar(util, avar);
            return function() {
                AVar.deleteCell(cell);
            };
        };
    }
    // output/Effect.AVar/index.js
    var Killed = /* @__PURE__ */ function() {
        function Killed2(value0) {
            this.value0 = value0;
        }
        Killed2.create = function(value0) {
            return new Killed2(value0);
        };
        return Killed2;
    }();
    var Filled = /* @__PURE__ */ function() {
        function Filled2(value0) {
            this.value0 = value0;
        }
        Filled2.create = function(value0) {
            return new Filled2(value0);
        };
        return Filled2;
    }();
    var Empty2 = /* @__PURE__ */ function() {
        function Empty3() {}
        Empty3.value = new Empty3();
        return Empty3;
    }();
    var ffiUtil2 = /* @__PURE__ */ function() {
        return {
            left: Left.create,
            right: Right.create,
            nothing: Nothing.value,
            just: Just.create,
            killed: Killed.create,
            filled: Filled.create,
            empty: Empty2.value
        };
    }();
    var put2 = function(value12) {
        return function(avar) {
            return function(cb) {
                return _putVar(ffiUtil2, value12, avar, cb);
            };
        };
    };
    var take3 = function(avar) {
        return function(cb) {
            return _takeVar(ffiUtil2, avar, cb);
        };
    };
    // output/Effect.Aff.AVar/index.js
    var liftEffect7 = /* @__PURE__ */ liftEffect(monadEffectAff);
    var take4 = function(avar) {
        return makeAff(function(k) {
            return function __do2() {
                var c = take3(avar)(k)();
                return effectCanceler(c);
            };
        });
    };
    var put3 = function(value12) {
        return function(avar) {
            return makeAff(function(k) {
                return function __do2() {
                    var c = put2(value12)(avar)(k)();
                    return effectCanceler(c);
                };
            });
        };
    };
    var empty8 = /* @__PURE__ */ liftEffect7(empty7);
    // output/Halogen.Portal/index.js
    var bind8 = /* @__PURE__ */ bind(bindHalogenM);
    var lift22 = /* @__PURE__ */ lift(monadTransHalogenM);
    var get3 = /* @__PURE__ */ get(monadStateHalogenM);
    var pure11 = /* @__PURE__ */ pure(applicativeHalogenM);
    var hoist4 = /* @__PURE__ */ hoist3(functorAff);
    var forever2 = /* @__PURE__ */ forever(monadRecHalogenM);
    var discard6 = /* @__PURE__ */ discard(discardUnit)(bindHalogenM);
    var modify_4 = /* @__PURE__ */ modify_2(monadStateHalogenM);
    var for_3 = /* @__PURE__ */ for_(applicativeHalogenM)(foldableMaybe);
    var gets2 = /* @__PURE__ */ gets(monadStateHalogenM);
    var map25 = /* @__PURE__ */ map(functorAff);
    var slot2 = /* @__PURE__ */ slot();
    var ntIdentity = /* @__PURE__ */ identity(categoryFn);
    var component2 = function(dictMonadAff) {
        var MonadEffect0 = dictMonadAff.MonadEffect0();
        var lift3 = lift22(MonadEffect0.Monad0());
        var liftAff2 = liftAff(monadAffHalogenM(dictMonadAff));
        var liftEffect8 = liftEffect(monadEffectHalogenM(MonadEffect0));
        return function(contextualize) {
            var render = function(v) {
                return text5("");
            };
            var ioq = function(v) {
                return v.query;
            };
            var initialState = function(v) {
                return {
                    input: v.input,
                    child: v.child,
                    targetElement: v.targetElement,
                    io: Nothing.value
                };
            };
            var $$eval = function(v) {
                if (v instanceof Initialize) return bind8(lift3(contextualize))(function(v1) {
                    return bind8(get3)(function(state3) {
                        return bind8(liftAff2(empty8))(function($$var2) {
                            return bind8(maybe(liftAff2(awaitBody))(pure11)(state3.targetElement))(function(target6) {
                                return bind8(liftAff2(runUI2(hoist4(v1)(state3.child))(state3.input)(target6)))(function(io) {
                                    return bind8(liftEffect8(subscribe(io.messages)(function(msg) {
                                        return launchAff(put3(msg)($$var2));
                                    })))(function() {
                                        return bind8(fork2(forever2(bind8(liftAff2(take4($$var2)))(function(msg) {
                                            return $$eval(new Action2(msg, unit));
                                        }))))(function() {
                                            return discard6(modify_4(function(v2) {
                                                var $115 = {};
                                                for(var $116 in v2)if (({}).hasOwnProperty.call(v2, $116)) $115[$116] = v2[$116];
                                                $115.io = new Just(io);
                                                return $115;
                                            }))(function() {
                                                return pure11(v.value0);
                                            });
                                        });
                                    });
                                });
                            });
                        });
                    });
                });
                if (v instanceof Finalize) return bind8(get3)(function(state3) {
                    return discard6(for_3(state3.io)(function($129) {
                        return liftAff2(function(v1) {
                            return v1.dispose;
                        }($129));
                    }))(function() {
                        return pure11(v.value0);
                    });
                });
                if (v instanceof Receive) return pure11(v.value1);
                if (v instanceof Action2) return discard6(raise(v.value0))(function() {
                    return pure11(v.value1);
                });
                if (v instanceof Query) return bind8(gets2(function(v1) {
                    return v1.io;
                }))(function(v1) {
                    if (v1 instanceof Nothing) return pure11(v.value1(unit));
                    if (v1 instanceof Just) return liftAff2(unCoyoneda(function(k) {
                        return function(q2) {
                            return map25(maybe$prime(v.value1)(k))(ioq(v1.value0)(q2));
                        };
                    })(v.value0));
                    throw new Error("Failed pattern match at Halogen.Portal (line 244, column 13 - line 246, column 87): " + [
                        v1.constructor.name
                    ]);
                });
                throw new Error("Failed pattern match at Halogen.Portal (line 212, column 10 - line 246, column 87): " + [
                    v.constructor.name
                ]);
            };
            return mkComponent({
                initialState,
                render,
                "eval": $$eval
            });
        };
    };
    var portal = function() {
        return function(dictIsSymbol) {
            var slot1 = slot2(dictIsSymbol);
            return function(dictOrd) {
                var slot22 = slot1(dictOrd);
                return function(dictMonadAff) {
                    var component1 = component2(dictMonadAff);
                    return function(contextualize) {
                        return function(label5) {
                            return function(slot3) {
                                return function(childComponent) {
                                    return function(childInput) {
                                        return function(htmlElement) {
                                            return function(handler3) {
                                                return slot22(label5)(slot3)(component1(contextualize))({
                                                    child: childComponent,
                                                    input: childInput,
                                                    targetElement: htmlElement
                                                })(handler3);
                                            };
                                        };
                                    };
                                };
                            };
                        };
                    };
                };
            };
        };
    };
    var portal1 = /* @__PURE__ */ portal();
    var portalAff = function() {
        return function(dictIsSymbol) {
            var portal2 = portal1(dictIsSymbol);
            return function(dictOrd) {
                var portal3 = portal2(dictOrd);
                return function(dictMonadAff) {
                    return portal3(dictMonadAff)(pure(dictMonadAff.MonadEffect0().Monad0().Applicative0())(ntIdentity));
                };
            };
        };
    };
    // output/Parent/index.js
    var discard7 = /* @__PURE__ */ discard(discardUnit)(bindHalogenM);
    var bindFlipped8 = /* @__PURE__ */ bindFlipped(bindHalogenM);
    var traverse_7 = /* @__PURE__ */ traverse_(applicativeHalogenM)(foldableMaybe);
    var childIsSymbol = {
        reflectSymbol: function() {
            return "child";
        }
    };
    var request2 = /* @__PURE__ */ request()(childIsSymbol)(ordUnit);
    var identity10 = /* @__PURE__ */ identity(categoryFn);
    var portalAff2 = /* @__PURE__ */ portalAff()(childIsSymbol)(ordUnit);
    var HandleChild = /* @__PURE__ */ function() {
        function HandleChild2(value0) {
            this.value0 = value0;
        }
        HandleChild2.create = function(value0) {
            return new HandleChild2(value0);
        };
        return HandleChild2;
    }();
    var _child = /* @__PURE__ */ function() {
        return $$Proxy.value;
    }();
    var component3 = function(dictMonadAff) {
        var monadEffectHalogenM2 = monadEffectHalogenM(dictMonadAff.MonadEffect0());
        var log4 = log3(monadEffectHalogenM2);
        var logShow3 = logShow2(monadEffectHalogenM2)(showInt);
        var portalAff1 = portalAff2(dictMonadAff);
        var handleAction = function(v) {
            return discard7(log4("clicked"))(function() {
                return bindFlipped8(traverse_7(logShow3))(request2(_child)(unit)(GetCount.create));
            });
        };
        return mkComponent({
            initialState: identity10,
            render: function(v) {
                return div2([])([
                    text5("I'm the parent"),
                    portalAff1(_child)(unit)(component)(unit)(Nothing.value)(HandleChild.create)
                ]);
            },
            "eval": mkEval({
                handleAction,
                handleQuery: defaultEval.handleQuery,
                receive: defaultEval.receive,
                initialize: defaultEval.initialize,
                finalize: defaultEval.finalize
            })
        });
    };
    // output/Main/index.js
    var $$void7 = /* @__PURE__ */ $$void(functorAff);
    var component4 = /* @__PURE__ */ component3(monadAffAff);
    var main2 = /* @__PURE__ */ runHalogenAff(/* @__PURE__ */ bind(bindAff)(awaitBody)(function(body2) {
        return $$void7(runUI2(component4)(unit)(body2));
    }));
    // <stdin>
    main2();
})();

},{}]},["20dt5","ck57E"], "ck57E", "parcelRequire94c2")

//# sourceMappingURL=index.js.map
