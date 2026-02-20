;;; early-init.el --- Pre-frame initialization -*- lexical-binding: t; -*-

;; Defer GC during startup (reset in init.el)
(setq gc-cons-threshold most-positive-fixnum)

;; We handle package initialization ourselves
(setq package-enable-at-startup nil)

;; Prevent frame resizing during font/UI setup
(setq frame-inhibit-implied-resize t)

;; Suppress UI elements before frame is drawn
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
;; Keep menu-bar (user preference)
(push '(menu-bar-lines . 1) default-frame-alist)

;;; early-init.el ends here
