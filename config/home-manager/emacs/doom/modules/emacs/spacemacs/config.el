;;; emacs/spacemas/config.el -*- lexical-binding: t; -*-

(use-package evil-iedit-state
  :config
  (defalias 'iedit-lib-cleanup 'iedit-cleanup))

(map! :leader
      :desc "Activate iedit state" "s e" 'evil-iedit-state/iedit-mode
      :desc "Switch to alternate buffer" "TAB" 'spacemacs/alternate-buffer
      :desc "Clear vim search highlights" "s c" 'spacemacs/evil-search-clear-highlight)

(map!
 :desc "Winner Undo" "C-c <left>" 'winner-undo
 :desc "Winner Redo" "C-c <right>" 'winner-redo)
