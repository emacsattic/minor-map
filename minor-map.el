;; Minor mode keymap for Emacs

;; Use minor-set-key to set a key in the minor keymap for a given
;; minor mode. 
;;
;; Use unbind-minor-mode to remove all key definitions for a given
;; minor mode.

;; COPYLEFT

;; Created 1987 by Per Abrahamsen at University of Aalborg, Denmark.
;; Updated 1991 by Per Abrahamsen at University of Aalborg, Denmark.
;; This file is not part of GNU emacs (Yet... ;-)

;; License: public-domain

;; Everyone is granted permission to copy, modify and redistribute
;; this file, this includes the right to remove this message.

;; HOW MINOR-MAP WORKS!

;; GNU Emacs minor modes may not change the keymap.  This is why modes
;; like outline-mode and indented-text-mode are major modes.
;; Minor modes could of cause just change the local key binding on entry,
;; and then restore it on exit.  But this will fail, if some other
;; minor mode changes the keybinding meanwhile.
;;
;; The idea behind minor-map is to keep a list of which minor mode has
;; what binding for a particular key.  The list is sorted
;; chronologically, with the most recently enabled minor mode first,
;; and the major mode last.  The key will always be bound according to
;; the head of the list, that is, the last enabled minor mode.  When a
;; minor mode is disabled, it will be removed from the list, and if it
;; was at the head of the list the key will be bound according to the
;; next entry in the list. 
;;
;; When a minor mode is enabled, it should bind all its keys with
;; minor-set-key, and, when disabled, unbound them with
;; unbind-minor-mode. 
;;
;; The minor mode keymaps are contained in a local variable
;; minor-keymap, described in the documentation of make-minor-keymap.

;; RESTRICTIONS

;; The minor maps might fail, if the major mode (or the user) change
;; the local key bindings after the first call to minor-set-key.
;;
;; Minor maps should support shared keymaps for prefix keys.

;; DOES MINOR-MAP WORK?

;; Please report any bug reports (or a formal proof of correctness) to
;; abraham@iesd.auc.dk

(provide 'minor-map)

(make-variable-buffer-local 'minor-keymap) ;Documentation?
(setq-default minor-keymap nil)

(make-variable-buffer-local 'minor-local-keymap)
(setq-default minor-local-keymap nil)

(defun make-minor-keymap ()
  "Construct a keymap for minor modes if none exists.

The keymap is an alist of (KEY . MODE-LIST) where MODE-LIST is a list
of (NAME . COMMAND).

Each KEY represents an entry to the alist.  If a key sequence have no
entry, it mean that the KEY is bound to the default value in the
major mode map.

NAME identify a minor mode, that have bound KEY to COMMAND.
The head of MODE-LIST represent the newest, and currently active,
binding.  The NAME of the tail is the major-mode, and COMMAND is
the default binding. The MODE-LIST always contains at least the
major-mode entry.

Initially the alist is nil."
  
  ;Try to make keymap buffer-local, *not* mode-local
  (if (current-local-map)
      (setq minor-local-keymap (copy-keymap (current-local-map)))
    (setq minor-local-keymap (make-sparse-keymap)))
  (use-local-map minor-local-keymap))

(defun minor-set-key (key command name)
  "Bind KEY to COMMAND in the minor keymap used by the minor mode NAME.
NAME is a symbol that must identify the minor mode."

  (if (null minor-local-keymap)
      (make-minor-keymap))
  (let ((map-entry (assoc key minor-keymap)))
    (if (null map-entry)
	(progn 
	  (setq map-entry (list key
				(cons major-mode
				      (local-key-binding key))))
	  (setq minor-keymap (cons map-entry
				   minor-keymap))))
    (rplacd map-entry (cons (cons name command)
			    (cdr map-entry))))
  (local-set-key key command))

(defun unbind-minor-mode (name)
  "Remove all references to minor mode NAME from the minor keymap.
Key bindings will be updated to reflect the new content of the minor
keymap."

  (mapcar 'unbind-minor-entry minor-keymap))

(defun unbind-minor-entry (entry)
  "Remove any references to a minor mode from ENTRY. ENTRY have the
form (KEY . MODE-LIST) where MODE-LIST is a list of (NAME . COMMAND).
NAME is compared with the value of the variable \"name\", and the
conscell is removed if equal.  If the first entry in MODE-LIST is
removed, KEY is rebound to the COMMAND of the next cell."

  (let ((current (cdr entry))
	(previous entry))
    (if (eq name (car (car current)))
	(local-set-key (car entry) (cdr (car (cdr current)))))
    (while current
      (if (eq name (car (car current)))
	    (rplacd previous (cdr current))
	(setq previous current))
      (setq current (cdr current)))))
