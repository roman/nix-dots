;;; ui/golden/config.el -*- lexical-binding: t; -*-

(use-package! golden-ratio
  :config
  (setq golden-ratio-auto-scale t))

(map! :leader
        :desc "Toggle golden-ratio"
        "t G" #'golden-ratio-mode)
