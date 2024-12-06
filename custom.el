(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list
   '(("LaTeXMkClean" "latexmk -c %(latexmk-out) %(file-line-error) %(output-dir) %`%(extraopts) %S%(mode)%' %t" TeX-run-TeX nil
      (LaTeX-mode)
      :help "latexmk clean")
     ("LaTeXMkCompile" "latexmk -pdf %(latexmk-out) %(file-line-error) %(output-dir) %`%(extraopts) %S%(mode)%' %t" TeX-run-TeX nil
      (LaTeX-mode)
      :help "latexmk compile")
     ("TeX" "%(PDF)%(tex) %(file-line-error) %`%(extraopts) %S%(PDFout)%(mode)%' %(output-dir) %t" TeX-run-TeX nil
      (plain-TeX-mode Texinfo-mode AmSTeX-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %T" TeX-run-TeX nil
      (LaTeX-mode docTeX-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %(o-dir) %t" TeX-run-compile nil
      (Texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) %(o-dir) --html %t" TeX-run-compile nil
      (Texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "amstex %(PDFout) %`%(extraopts) %S%(mode)%' %(output-dir) %t" TeX-run-TeX nil
      (AmSTeX-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "%(cntxcom) --once %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (ConTeXt-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (ConTeXt-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %(O?aux)" TeX-run-BibTeX nil
      (plain-TeX-mode LaTeX-mode docTeX-mode ConTeXt-mode Texinfo-mode AmSTeX-mode)
      :help "Run BibTeX")
     ("Biber" "biber %(output-dir) %s" TeX-run-Biber nil
      (plain-TeX-mode LaTeX-mode docTeX-mode Texinfo-mode AmSTeX-mode)
      :help "Run Biber")
     ("Texindex" "texindex %s.??" TeX-run-command nil
      (Texinfo-mode)
      :help "Run Texindex")
     ("Texi2dvi" "%(PDF)texi2dvi %t" TeX-run-command nil
      (Texinfo-mode)
      :help "Run Texi2dvi or Texi2pdf")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t
      (plain-TeX-mode LaTeX-mode docTeX-mode Texinfo-mode AmSTeX-mode)
      :help "Generate PostScript file")
     ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil
      (plain-TeX-mode LaTeX-mode docTeX-mode Texinfo-mode AmSTeX-mode)
      :help "Convert DVI file to PostScript")
     ("Dvipdfmx" "dvipdfmx -o %(O?pdf) %d" TeX-run-dvipdfmx nil
      (plain-TeX-mode LaTeX-mode docTeX-mode Texinfo-mode AmSTeX-mode)
      :help "Convert DVI file to PDF with dvipdfmx")
     ("Ps2pdf" "ps2pdf %f %(O?pdf)" TeX-run-ps2pdf nil
      (plain-TeX-mode LaTeX-mode docTeX-mode Texinfo-mode AmSTeX-mode)
      :help "Convert PostScript file to PDF")
     ("LaTeXMk" "latexmk %(latexmk-out) %(file-line-error) %(output-dir) %`%(extraopts) %S%(mode)%' %t" TeX-run-format nil
      (LaTeX-mode docTeX-mode)
      :help "Run LaTeXMk")
     ("Glossaries" "makeglossaries %(d-dir) %s" TeX-run-command nil
      (plain-TeX-mode LaTeX-mode docTeX-mode Texinfo-mode AmSTeX-mode)
      :help "Run makeglossaries to create glossary file")
     ("Index" "makeindex %(O?idx)" TeX-run-index nil
      (plain-TeX-mode LaTeX-mode docTeX-mode Texinfo-mode AmSTeX-mode)
      :help "Run makeindex to create index file")
     ("upMendex" "upmendex %(O?idx)" TeX-run-index t
      (plain-TeX-mode LaTeX-mode docTeX-mode Texinfo-mode AmSTeX-mode)
      :help "Run upmendex to create index file")
     ("Xindy" "texindy %s" TeX-run-command nil
      (plain-TeX-mode LaTeX-mode docTeX-mode Texinfo-mode AmSTeX-mode)
      :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (LaTeX-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (LaTeX-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")))
 '(custom-safe-themes
   '("691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b" "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7" "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "d6b934330450d9de1112cbb7617eaf929244d192c4ffb1b9e6b63ad574784aad" "7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" default))
 '(package-selected-packages
   '(nix-ts-mode treesit nix-mode emmet-mode tsx-mode cdlatex auctex tree-sitter-langs treemacs yasnippet org-mode which-key web-mode vertico unicode-fonts treemacs-nerd-icons treemacs-evil toc-org rust-mode peep-dired org-superstar org-roam org-auto-tangle nerd-icons-dired nerd-icons-corfu magit general evil-vimish-fold evil-org evil-commentary evil-collection evil-anzu elpy doom-themes doom-modeline dockerfile-mode diredfl dashboard corfu cape))
 '(package-vc-selected-packages '((org-mode :url "https://code.tecosaur.net/tec/org-mode"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
