<!-- -*-coding: utf-8 --* -->

MOTIVATION
==========

In Emacs Lisp (elisp), there is no native namespace system. While it
has the convenient consequence, that the same symbol means the same
everywhere (a boon for interactive features like `M-x
describe-function` or `M-x find-function`), it also results in a lot
of “boilerplate” re-typing of namespace prefix strings.

While implementations of macro-based namespaces such as `names.el`
exist, they are bound to lack support for current or future
interactive features, that depend on the presence of full function
names in the source-code files.

This provisional package is meant to make working with namespace
prefices more convenient in the meantime.

It is also meant for experimenting with what works.



USAGE
=====

  - Download `myelisp-namespace-helper.el`.
  
  - Either
  
      * Copy/paste the contents into your `.emacs` file, or
        (preferably)
      * Put a statement
      
            (load-file "PATH/TO/myelisp-namespace-helper.el")
        
        into your `.emacs` file.
  
  - Enable `M-x myelisp-namespace-helper-mode` in buffers, where you
    want to use it. Optionally, enable `M-x
    myelisp-namespace-helper-global-mode`.
  
  - In `emacs-lisp-mode` or `lisp-interaction-mode`, insert a
    buffer-appropriate namespace prefix by pressing `-` once, while
    not already inside a symbol, and again for a private prefix
    according to the `NAMESPACE-public-name` and
    `NAMESPACE--private-name` covention.
    
    The `-`-key doesn't insert a prefix
    
      * when typed after symbol characters,
      * when immediately followed by a number or whitespace,
      * when typed inside a string or comment, unless preceded by a
        backtick (`` ` ``) or quote (`'`) character, or
      * when inserted using `quoted-insert` (normally `C-q -`).

  - Define a file variable `myelisp-namespace-helper-prefix` to override the
    default prefix.
    
With this code, `prettify-symbols-mode` collapses the namespace
prefix into a highlighted hyphen, mirroring the input method.

For LaTeX, the `@` character is expanded to `NAMESPACE@`, corresponding
to the common convention for internal variables and macros.
By default, `myelisp-namespace-helper-global-mode` affects only
Emacs Lisp buffers. This behavior can be changed by customizing
`myelisp-namespace-helper-affected-modes-alist`
via <kbd>M-x customize-variable</kbd>.


CHANGELOG
=========

**2020-02-06** In order to support ``dash.el``, the namespace prefix will
be removed, if doing so results in a known function or variable.

**2018-06-26** Added default entries for ``doctex-mode`` (i.e. *.dtx*-files).

**2018-06-23.2** Made prefix character, and whether modes are affected
by `myelisp-namespace-helper-global-mode` configurable via
`M-x customize-variable myelisp-namespace-helper-affected-modes-alist`.

**2018-06-23.1** Added support for LaTeX with the `@` prefix character.

**2018-06-14**
  - Created proper repository with separate `README.md` file from gist.
  - Added line that properly activates global mode if enabled
    in the customization interface.

**2018-06-12** Rewrote in terms of the `post-self-insert-hook`. Sadly,
on the way the limited support for the `eval-expression` as only case
of minibuffer usage was lost too.

The improvement, that now `-1` or `(- 1 2)` can be typed without
producing a prefix, is more important however.

Also merged CHANGELOG into the source file.

**2018-06-10** Bugfix: Limited the faces that affect the `-` key;
Previously `show-paren-match` face prevented insertion when cursor
located immediately before a `(`.

**2018-06-08** More documentation.

**2018-06-07** Made file namespace-clean.



TODO: IMPROVEMENTS?
===================

  - Prettification, and maybe insertion, should possibly be limited to
    `private--names` by default. The programmer should remain
    aware of how verbose client code will see the “public”
    interface.

  - This would also solve the issue of typing negative numbers
    or the `-` function without `C-q` prefix.
    
  - Need to find way to support minibuffer. I currently have no
    general way of telling generally, if the read-request to the
    minibuffer wants a symbol/expression as completion.


WHAT DID NOT WORK
=================

  - Key `~`. It is the least common printable ASCII character
    in Emacs's lisp code, but using the same key for inserting
    the prefix and for separating words in symbols proved a huge
    usability advantage.

  - Double `--` for prefix, triple `---` for private prefix.
    Possibly a matter of training, but it felt wrong.


TODO: FUTURE PLANS
==================

  - Separate minor mode from `prettify-symbols-mode`.
