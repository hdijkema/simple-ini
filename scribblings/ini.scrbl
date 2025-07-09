#lang scribble/manual

@(require
   scribble/example
   (for-label racket/base
              racket/string
              racket/file))

@;(define myeval
;   (make-base-eval '(require simple-ini)))

@title[#:tag "ini-parser"]{INI File Parser and Writer}

@author[@author+email["Hans Dijkema" "hans@dijkewijk.nl"]]

@defmodule[simple-ini]{This module provides simple facilities for reading and writing INI-style configuration files, allowing interpretation of numeric and boolean values, and modification of the parsed structure.}

@section{Creating and Parsing INI Files}

@defproc[(make-ini) mc-pair?]{
  Creates a new empty INI structure as a mutable cons pair. This is the base structure for manipulating INI contents.
}

@defproc[(file->ini [file path-string?]) mc-pair?]{
  Reads an INI file from disk and parses it into an internal mutable cons pair (mc-pair) structure.
  
  The parser supports:
  
  @itemlist[
    @item{Sections (e.g., @tt{[section-name]})}
    @item{Key-value pairs (e.g., @tt{key=value})}
    @item{Comments (lines starting with @tt{;})}
    @item{Empty lines}
  ]

  The keys are stored as symbols, and values are automatically interpreted as:
  @itemlist[
    @item{Numbers, if the value matches a number pattern}
    @item{Booleans, if the value is @tt{#t}, @tt{true}, @tt{#f}, or @tt{false} (case-insensitive)}
    @item{Otherwise, as strings}
  ]
}

@defproc[(ini->file [ini mc-pair?] [file path-string?]) void?]{
  Writes an INI structure (as produced by @racket[file->ini] or @racket[make-ini]) to the specified file.
  
  The output preserves:
  @itemlist[
    @item{Section headers}
    @item{Key-value pairs}
    @item{Comments (prefixed with @tt{;})}
    @item{Empty lines}
  ]
}

@section{Accessing and Modifying Values}

@defproc[(ini-get [ini mc-pair?]
                  [section symbol?]
                  [key symbol?]
                  [def-val any/c])
         any/c]{
  Retrieves the value associated with the given @racket[key] in the specified @racket[section] of the INI structure. If the key is not found, returns @racket[def-val].
}

@defproc[(ini-set! [ini mc-pair?]
                   [section symbol?]
                   [key symbol?]
                   [val any/c])
         mc-pair?]{
  Sets the value of @racket[key] in the specified @racket[section] of the INI structure. If the section or key does not exist, it is created. Returns the modified INI structure.
                    }

@section{The @racket[ini] Roos Class}

@defmodule[simple-ini/roos]{Require this module for the OO implementation of this Simple INI implementation}

@racket[(def-roos ini . file)]{
  A Roos class that provides object-oriented access to INI files using the underlying @racket[file->ini] parser system. The class offers methods to load, query, and update INI files using familiar object-style interactions.

  @defproc[
    (ini [file (or/c path-string? #f)] ...)
  ]{
    Creates an @racket[ini] object. If a @racket[file] path is provided and the file exists, it is loaded immediately. Otherwise, an empty INI structure is created.

    If no file is provided, the object operates in-memory only. Subsequent @racket[set!] operations will raise an error unless a file is later specified with @racket[(file!)].
  }

  @defmethod[(file) (or/c path-string? #f)]{
    Returns the current filename associated with this INI object.
  }

  @defmethod[(file! [f path-string?]) void?]{
    Sets the file to use as backing storage for the INI structure. Triggers a reload from disk.
  }

  @defmethod[(reload) void?]{
    Reloads the INI content from disk, using the current file path.
    If the file does not exist, the content is reset to an empty INI structure.
  }

  @defmethod[(set! [section symbol?] [key symbol?] [val any/c]) ini]{
    Sets the value in the INI structure for the given @racket[section] and @racket[key] to @racket[val].

    If a file is associated with the object, the structure is saved to disk immediately.
    If no file is set and @racket[fail] is enabled, an error is raised.
    Returns the INI object itself.
  }

  @defmethod[(get [section symbol?] [key symbol?] [def-val any/c] ...) any/c]{
    Retrieves the value associated with the given @racket[section] and @racket[key].
    
    If not found:
    @itemlist[
      @item{Returns @racket[#f] if no default is given and @racket[fail] is disabled}
      @item{Returns @racket[def-val] if one is provided}
      @item{Raises an error if @racket[fail] is enabled and no default is given}
    ]
  }

  @bold{Internal state:}
  @itemlist[
    @item{@racket[file*] — the filename (or @racket[#f])}
    @item{@racket[content] — the mutable INI structure}
    @item{@racket[fail] — when enabled, raises errors instead of returning fallback values}
  ]
}

