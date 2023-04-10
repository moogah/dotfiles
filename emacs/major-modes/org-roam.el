(require 'org-roam-protocol)

(server-start)

(setq org-roam-capture-templates
 '(("d" "default" plain
    "%?"
    :if-new (file+head "inbox/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed t)
   ("f" "foo" plain
    (file "~/.emacs.d/templates/org-roam-default.org")
    :if-new (file+head "inbox/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n ${body}")
    :unnarrowed t)))

(setq org-roam-capture-ref-templates
      '(("r" "ref" plain
        "%?"
        :target
        (file+head "${slug}.org" "#+title: ${title}\n${body}")
        :unnarrowed t)))
