SOURCE=../index.cgi ../statistics.scm ../registration.cgi ../process.scm

ja.edit.po: copl.pot ja.po
	msgmerge --output $@ -update $^

copl.pot: $(SOURCE)
	xgettext --from-code utf-8 -o $@ -L EmacsLisp $^

ja.po:
	msgcat --no-location --output $@ ja.edit.po
