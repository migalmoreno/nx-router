

# nx-router

`nx-router` is a declarative URL routing extension for [Nyxt](https://nyxt.atlas.engineer/). In short, it's an abstraction around Nyxt request resource handlers that uses `router` objects to make it more convenient to handle routes. See [Examples](#org967ab09) for a walk-through on how to set up routers.  

The main drive behind `nx-router` was I initially found built-in handlers difficult to reason and I soon became frustrated how much imperative logic I had to maintain. `nx-router` aims to simplify resource handling in Nyxt with declarative redirects, blockers, and resource handlers. You may think of it as a more batteries-included `url-dispatching-handler`.  

[nx-freestance-handler](https://github.com/kssytsrk/nx-freestance-handler) is a similar extension that provides handlers for popular privacy-friendly front-ends. However, this has the limitation that it only works with a few sites and makes you  reliant on its maintainer updating the extension to add new handlers or modify existing ones.  


## Installation

To install the extension, you need to download the source and place it in Nyxt's extensions path, given by the value of `nyxt-source-registry` (by default `~/.local/share/nyxt/extensions`).  

    git clone https://github.com/migalmoreno/nx-router ~/.local/share/nyxt/extensions/nx-router

The extension works with **Nyxt 3 onward** but it's encouraged to use it with the latest version of Nyxt master for the time being.  

If you want to place the extension elsewhere in the system, such as for development purposes, you can configure so via the ASDF source registry mechanism. For this, you'll need to create a file in the source registry directory, `~/.config/common-lisp/source-registry.conf.d/`, and then put the following contents into it, replacing the path with the desired system path.  

    (:tree "/path/to/user/location")

Then, make sure to refresh the ASDF cache via `asdf:clear-source-registry`. ASDF will now be able to find the extension on the custom path. For more information on this utility, please refer to the [ASDF manual](https://asdf.common-lisp.dev/asdf.html).  

By default, Nyxt won't read the custom source registry path we provided, so ensure to include a `reset-asdf-registries` invocation in Nyxt's configuration file too.  

In your Nyxt configuration file, place the following.  

    (define-nyxt-user-system-and-load nyxt-user/router
      :depends-on (nx-router)
      :components ("router.lisp"))

Where `router.lisp` is a custom file that should you should create relative to Nyxt's configuration directory (`*config-file*`'s pathname by default) to provide the extension settings after the `nx-router` system has been successfully loaded. Inside this file, place the following.  

    (define-configuration web-buffer
      ((default-modes `(router:router-mode ,@%slot-value%))))

In addition, you should add the extension options, explained in the following section.  


## Configuration

This shows some example routers you'd set up inside the `router-mode` configuration class:  

    (define-configuration router:router-mode
      ((router:routers
        (list
         (make-instance 'router:redirector
                        :route (match-domain "example.org")
                        :redirect
                        '(("https://acme.org/example" . (not ".*/$" ".*/wiki$"))))
         (make-instance 'router:blocker
                        :name 'wonka
                        :route (match-hostname "www.wonka.inc")
                        :instances-builder
                        (make-instance 'router:instances-builder
                                       :source "https://www.wonka.inc/instances.json"
                                       :builder (lambda (instances)
                                                  (json:decode-json-from-string
                                                   instances)))
                        :blocklist "/factory")
         (make-instance 'router:opener
                        :name 'wonka
                        :resource "mpv --video=no ~a")
         (make-instance 'router:redirector
                        :name 'wonka
                        :redirect
                        '(("https://\\1.acme.org/\\2" . ".*/p/(\\w+)/(.*)")))))))

The first router simply redirects all requests that match the domain <https://example.org> to <https://acme.org>, and adds a redirect rule so that any path of <https://example.org> that doesn't match `/` or `/wiki` will get redirected to <https://acme.org/example>. The second router sets a blocklist for the `/factory` PCRE, and the rest of the routers are a composition of the second router, so they inherit its settings.  

All routers derive from a `router` parent class that holds common router settings:  

-   **`name`:** a symbol for the name of the router which allows for router composition.
-   **`route`:** the route to match for `router` activation, akin to the predicates used in Nyxt `auto-rules`. One of `match-domain`, `match-host`, `match-regex`, `match-port`, a user-defined function, or a PCRE.
-   **`instances-builder`:** this takes an `instances-builder` object, which in turn takes a source to retrieve instances from and a builder which assembles them into a list.
-   **`toplevel-p` (default: `t`):** whether the router is meant to process only top-level requests.

Do note the WebkitGTK renderer poses some limitations with requests. For example, some of them might not get processed because click events are obscured, and `iframes` cannot be redirected at all. To alleviate this, you can control whether only top-level requests should be processed . If you want all requests to be processed, including non top-level ones, for all routers by default you can configure the `router` user-class like this:  

    (define-configuration router:router
      ((router:toplevel-p nil)))

If you'd like to process non top-level requests only for `redirector` and `opener` routers by default, you can configure them like this:  

    (define-configuration (router:redirector router:opener)
      ((router:toplevel-p nil)))

Finally, if you'd like to process non top-level requests only for a given instance of a `redirector` class, add this on router instantiation:  

    (make-instance 'router:redirector
                   :route (match-domain "example.org")
                   :redirect "acme.org"
                   :toplevel-p nil)

`redirector` is a redirect router that takes the following direct slots:  

-   **`redirect`:** a string for a URL hostname to redirect to, a `quri:uri` object for a complete URL to redirect to, a PCRE (used as the replacement string of `ppcre:regex-replace` against `route`), or an association list of redirection rules. In the latter, each entry is a cons of the form `REDIRECT . ROUTES`, where `ROUTES` is a list of regexps from the `route` that will be matched against and redirected to `REDIRECT`. To redirect all paths except `ROUTES` to `REDIRECT`, prefix this list with `not`.
-   **`reverse`:** a string for the router's original host or a `quri:uri` object for the original complete URL. This is useful for storage purposes (bookmarks, history, etc.) so that the original URL is recorded instead of the redirect's URL.

`blocker` is a blocking router that takes the following direct slots:  

-   **`block-banner-p` (default: `t`):** whether to display a block banner upon blocking the route.
-   **`blocklist`:** A PCRE to match against the current route, `t` to block the entire route, or a list of regexps to draw the comparison against. If any single list is prefixed with `not`, the entire route will be blocked except for the specified regexps. If all of the lists are prefixed with `or`, this follows an exception-based blocking where you can specify a more general block regexp first and bypass it for more specific routes.

`opener` is a router that instructs resources to be opened externally. It takes the following direct slots:  

-   **`resource`:** a resource can be either a function form, in which case it takes a single parameter URL and can invoke arbitrary Lisp forms with it.  If it's a string, it runs the specified command via `uiop:run-program` with the current URL as argument, and can be given in a `format`-like syntax.


## Examples

Redirect YouTube requests that match certain regexps to their corresponding [Tubo](https://github.com/migalmoreno/tubo) counterparts.  

    (make-instance 'router:redirector
                   :route (match-domain "youtube.com")
                   :redirect
                   '(("https://tubo.migalmoreno.com/stream?url=\\&" . (".*/watch\\?v.*" ".*/shorts/.*"))
                     ("https://tubo.migalmoreno.com/playlist?list=\\&" . ".*/playlist/.*")
                     ("https://tubo.migalmoreno.com/channel?url=\\&" . ".*/channel/.*")))

Set up a redirect for all Instagram requests except those that match the regexps `.*/tv/.*` or `.*/reels/.*` to <https://picuki.com/profile/>, and redirect routes that match the regexp `.*/p/(.*)` to <https://pickuki.com/media/\\1>, where the replacement string `\\1` will be replaced by the `(.*)` capture group in the URL.  

    (make-instance 'router:redirector
                   :route (match-regex "https://(www.)?insta.*")
                   :redirect
                   '(("https://picuki.com/profile/" . (not ".*/tv/.*" ".*/reels/.*"))
                     ("https://pickuki.com/media/\\1" . ".*/p/(.*)")))

Redirect all TikTok requests except the index path, videos, or usernames to <https://tok.artemislena.eu/@placeholder/video/>, and redirect the rest of routes to <https://tok.artemislena.eu/\\1>, where the replacement string `\\1` will be replaced by everything following the original hostname in the URL. Additionally, block all the routes except those that contain the `.*/video/.*` or the `.*/t/.*` regexps.  

    (list
     (make-instance 'router:redirector
                    :name 'tiktok
                    :route (match-domain "tiktok.com")
                    :redirect
                    '(("https://tok.artemislena.eu/@placeholder/video/" . (not ".*/@.*" ".*/t/.*"))
                      ("https://tok.artemislena.eu/\\1" . ".*://[^/]*/(.*)$")))
     (make-instance 'router:blocker
                    :name 'tiktok
                    :blocklist '(or (not ".*/video/.*") (not ".*/t/.*"))))

You can pass a `:reverse` slot to the `redirector` router to perform a reverse redirection of the route, which can be useful when recording your history, for instance. For this, you have to wrap Nyxt internal methods like this:  

    (defmethod nyxt:on-signal-load-finished :around ((mode nyxt/history-mode:history-mode) url)
      (call-next-method mode (router:trace-url url)))

Use a `redirector` router with a PCRE trigger to redirect all Fandom routes to your preferred [BreezeWiki](https://breezewiki.com/) instance.  

    (make-instance 'router:redirector
                   :route "https://([\\w'-]+)\\.fandom.com/wiki/(.*)"
                   :redirect "https://breezewiki.com/\\1/wiki/\\2")

Use a `redirector` router to match YouTube-like video URLs and MP3 files and redirect these to <https://www.youtube.com>, and dispatch an `opener` router that launches an [mpv](https://mpv.io/) player IPC client process through [mpv.el](https://github.com/kljohann/mpv.el) to control the player from Emacs. You can also pass a one-placeholder format string such as `mpv --video=no ~a` to the `resource` slot if you'd rather not use a Lisp form, where `~a` represents the matched URL.  

    (list
     (make-instance 'router:redirector
                    :name 'youtube
                    :route '((match-regex ".*/watch\\?v=.*")
                             (match-file-extension "mp3"))
                    :redirect "www.youtube.com")
     (make-instance 'router:opener
                    :name 'youtube
                    :resource (lambda (url)
                               (eval-in-emacs `(mpv-start ,url)))))

Pass an `instances-builder` to generate a list of instances that will be appended to the routes on router instantiation. Also provide `redirect` as a function to compute the redirect hostname to use. See [instances.lisp](instances.lisp) for some predefined builders for front-end providers.  

    (defun set-invidious-instance ()
      "Set the primary Invidious instance."
      (let ((instances
              (remove-if-not
               (lambda (instance)
                 (and (string= (alex:assoc-value (second instance)
                                                 :region)
                               "DE")
                      (string= (alex:assoc-value (second instance)
                                                 :type)
                               "https")))
               (json:with-decoder-simple-list-semantics
                 (json:decode-json-from-string
                  (dex:get
                   "https://api.invidious.io/instances.json"))))))
        (first (car instances))))
    
    (make-instance 'router:redirector
                   :route (match-domain "youtube.com" "youtu.be")
                   :redirect #'set-invidious-instance
                   :instances-builder router:invidious-instances-builder)

If you'd like to redirect a route to a URL with a scheme other than HTTPS or a non-standard port, you need to supply `redirect` as a `quri:uri` object. For instance, this sets up a router that redirects Google results to a locally-running [whoogle-search](https://github.com/benbusby/whoogle-search) instance and where results will appear as if they were searched in Google:  

    (make-instance 'router:redirector
                   :route (match-regex ".*://whoogle.*" ".*://.*google.com/search.*")
                   :redirect (quri:uri "http://localhost:5000")
                   :reverse (quri:uri "https://www.google.com"))

If you want to randomize your `redirect` between a list of hosts, you can use a service like [Farside](https://sr.ht/~benbusby/farside/) and write a router along these lines:  

    (make-instance 'router:redirector
                   :route (match-domain "twitter.com")
                   :redirect '(("https://farside.link/nitter/\\1" . ".*://[^/]*/(.*)$")))

Use a router with an exception-based `blocklist` for <https://github.com>. These rules allow you to specify two or more predicates to draw the block-list comparison against. In the example below, the first regex indicates we want to block routes that consist of a single path entry (e.g. `https://github.com/path`) **or** block routes except those that contain the `/pulls` or `/search` paths. This allows you to provide a general block rule and bypass it for specific routes.  

    (make-instance 'router:blocker
                   :route (match-domain "github.com")
                   :blocklist '(or ".*://[^/]*/[^/]*$" (not ".*/pulls.*" ".*/search.*")))


## Contributing

Feel free to open an issue with bug reports or feature requests. PRs are more than welcome too.  

