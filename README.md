Using it:

     (define-key global-map (kbd "C-x C-g") 'git-find-file)

Then 'C-x C-g' from within your project directory.  You'll see a list of the
files in your current Git repository, and anything you type will be used as
input to narrow the list.  A pattern like:

     => srcmarcmaclj

Could match files like:

     src/marcgrep/sources/marc_file.clj
     src/marcgrep/sources/marcxml_file.clj
     src/marcgrep/destinations/marcfile.clj

That is, the pattern is applied (very!) fuzzily, and any path where the
letters of the pattern appear in order will be returned.

There's some basic relevance ranking here: exact substring matches sort above
fuzzy matches, fuzzy matches against the path's basename are better than
matches against the entire path, and matches where the letters appear close
to one another are better than when they're spread out.

Hitting RET opens the file under point, and C-g will abort the file selection.

Finally, C-s "rotates" the list:

     /path/a              /path/b              /path/c
     /path/b  --[C-s]-->  /path/c  --[C-s]-->  /path/a
     /path/c              /path/a              /path/b


