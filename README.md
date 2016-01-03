<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgheadline1">1. Overview</a></li>
<li><a href="#orgheadline2">2. Installation and configuration</a></li>
<li><a href="#orgheadline3">3. Usage</a></li>
<li><a href="#orgheadline4">4. Motivation</a></li>
</ul>
</div>
</div>

# Overview<a id="orgheadline1"></a>

org-attach-screenshot allows taking screenshots from within an emacs org
buffer session.

**Important note:** This package was originally named
*org-screenshot*, but due to a name clash with one of the
org-contrib packages, I decided to rename it to
*org-attach-screenshot*.

Features:

-   The link to the image file will be placed at *point*
-   *org inline images* will be turned on to display it.
-   Screenshots are placed into the org entry's attachment
    directory.
-   If no attachment directory has been defined, the user will be
    offered choices for creating one or using a directory of an entry
    higher up in the hierarchy.
-   The emacs frame from which the command is issued will hide away
    during the screenshot taking, except if a prefix argument has been
    given (so to allow taking images of the emacs session itself).
    
    ![img](fig/figure1.png)

# Installation and configuration<a id="orgheadline2"></a>

Put org-attach-screenshot.el into your load-path and the following into
your `~/.emacs`:

    (require 'org-attach-screenshot)

You can customize the command that is used for taking the screenshot
by configuring the `org-attach-screenshot-command-line` variable.

    (setq org-attach-screenshot-command-line "mycommand -x -y -z %f")

By default the `import` command from the ImageMagick suite is used, i.e.
the variable is set to "import %f".

# Usage<a id="orgheadline3"></a>

While in an org mode buffer, use the **org-attach-screenshot** command to take a screenshot and
have it inserted at the current point.

If the custom variable **org-attach-screenshot-relative-links** is non-nil, the
links inserted in the org buffer will always be relative to the org
document's location. If the variable is set to nil, the links will
just be the concatenation of the attachment dir and the filename. So, if
absolute path names are desired, you should set this option to nil and
make sure that you specify absolute directory names for the attachment
directories.

# Motivation<a id="orgheadline4"></a>

Org with its Babel functionality is a great tool for producing technical
documentation. One can directly use code snippets to produce graphics and
have them rendered into the document. So, it is great for writing a manual
about an application which produces graphics output.

While working on my manual, I several times had to take screenshots in order
to illustrate how to interact with the program. I thought it would
be fantastic to have this functionality integrated with org attachments, since
this provides a means to associate the figure files with org entries, and
not having to copy files around or enter the path information again and again.
Also, I wanted to immediately see the results in my org buffer.

I think that org-attach-screenshot will also be useful for users of
the more agenda related strengths of org mode.

BTW: There is another nice generic screenshot library available for
emacs ([screenshot.el by rubikitch](http://www.emacswiki.org/emacs/screenshot.el)) with some great features, but
different focus (more generic, allows uploads of screenshots to
remote servers and offers multiple predefined storage targets). I used
it at first as a dependency of this module, but since I essentially ended up only
using the wrapper function for the ImageMagick import command, I decided
to rather make it independent instead of forcing users to install both.
I wanted to have an especially well integrated screenshot feature to
match the org workflow.