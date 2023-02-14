(require 'predictable)

(funcall (predictable-display-buffer-reuse-window-with-predicate (buf)
           (->> buf
                (buffer-name)
                (string-match-p (rx bot (or "*Help" "*helpful" "*info*")))))
         nil nil)
