(defvar zooclj-packages
  '(
    evil-paredit
    cider
    clojure
    ))

(defun zooclj/init-evil-paredit ())

(defun zooclj/pre-init-clojure-mode ()
  (spacemacs|use-package-add-hook emacs-lisp-mode
    :pre-init
    (progn
      (when (and (configuration-layer/package-usedp 'paredit-mode)
                 (configuration-layer/package-usedp 'evil-paredit-mode)
                 (configuration-layer/package-usedp 'smartparens-mode))
        (add-hook 'emacs-lisp-mode-hook 'zooclj/lisp-lang-after-hook)))))

(defun zooclj/pre-init-cider ()
  (spacemacs|use-package-add-hook cider
    :pre-init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'clojure-mode
        "l" 'zooclj/cider-switch-and-load
        "," 'zooclj/cider-load-and-test
        ">" 'paredit-forward-slurp-sexp
        "<" 'paredit-backward-slurp-sexp)
      (add-hook 'emacs-lisp-mode-hook 'zooclj/lisp-lang-after-hook))))

(defun zooclj/pre-init-clojure-mode ()
  (spacemacs|use-package-add-hook clojure-mode
    :pre-init
    (progn
      (when (configuration-layer/package-usedp 'cider)
        (setq cider-repl-pop-to-buffer-on-connect t))
      (when (and (configuration-layer/package-usedp 'paredit-mode)
                 (configuration-layer/package-usedp 'evil-paredit-mode)
                 (configuration-layer/package-usedp 'smartparens-mode))
        (add-hook 'clojure-mode-hook 'zooclj/clojure-after-hook)))))
