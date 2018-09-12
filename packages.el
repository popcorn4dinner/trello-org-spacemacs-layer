;;; packages.el --- trello Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar trello-packages
  '(
    org-trello
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar trello-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function trello/init-<package-trello>
;;
(defun trello/init-org-trello ()
  (use-package org-trello
    :commands (org-trello/version
               org-trello/install-key-and-token
               org-trello/install-board-metadata
               org-trello/sync-card
               org-trello/sync-buffer
               org-trello/assign-me
               org-trello/check-setup
               org-trello/delete-setup
               org-trello/create-board-and-install-metadata
               org-trello/kill-entity
               org-trello/kill-cards
               org-trello/archive-card
               org-trello/archive-cards
               org-trello/jump-to-trello-card
               org-trello/jump-to-trello-board
               org-trello/add-card-comments
               org-trello/show-card-comments
               org-trello/show-card-labels
               org-trello/update-board-metadata
               org-trello/help-describing-bindings
               )
    :init
    ;; org-trello major mode for all .trello files
    (add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))

    ;; add a hook function to check if this is trello file, then activate the org-trello minor mode.
    (add-hook 'org-mode-hook
              (lambda ()
                (let ((filename (buffer-file-name (current-buffer))))
                  (when (and filename (string= "trello" (file-name-extension filename)))
                    (org-trello-mode)))))

    :config
    (progn
      (dolist (prefix '(("t" . "trello")
                        ("tb" . "board")
                        ("tc" . "card")
                        ("tcc" . "comments")
                        ("tj" . "jump")
                        ("ts" . "setup")))
        (spacemacs/declare-prefix-for-mode 'org-mode (car prefix) (cdr prefix)))

      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "tbs"  'org-trello/sync-buffer
        "tbn"  'org-trello/create-board-and-install-metadata
        "tbu"  'org-trello/update-board-metadata
        "tbi"  'org-trello/install-board-metadata
        "tcs"  'org-trello/sync-card
        "tcd"  'org-trello/archive-card
        "tcD"  'org-trello/archive-cards
        "tcd"  'org-trello/kill-entity
        "tcK"  'org-trello/kill-cards
        "tcm"  'org-trello/assign-me
        "tccs" 'org-trello/show-card-comments
        "tcca" 'org-trello/add-card-comments
        "tjb"  'org-trello/jump-to-trello-board
        "tjc"  'org-trello/jump-to-trello-card
        "tsc"  'org-trello/check-setup
        "tsd"  'org-trello/delete-setup
        "tsn"  'org-trello/install-key-and-token
        "tv"   'org-trello/version
        "th"   'org-trello/help-describing-bindings)

      (spacemacs/declare-prefix "jt" "trello")
      (evil-leader/set-key
        "jtb" 'org-trello/jump-to-trello-board
        "jtc" 'org-trello/jump-to-trello-card))))



