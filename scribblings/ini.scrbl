#lang scribble/manual

@(require
   scribble/example
   scribble/core
   (for-label racket/base
              racket/string
              racket/class
              racket/file))

(define myeval
   (make-base-eval '(require simple-ini roos)))

@title[#:tag "ini-parser"]{INI File Parser and Writer}

@author[@author+email["Hans Dijkema" "hans@dijkewijk.nl"]]

@defmodule[simple-ini]{This module provides simple facilities for reading and writing INI-style configuration files, allowing interpretation of numeric and boolean values, and modification of the parsed structure.}

@section{Creating and Parsing INI Files}

@defproc[(make-ini) mc-pair?]{
  Creates a new empty INI structure as a mutable cons pair. This is the base structure for manipulating INI contents.
}

@defproc[(file->ini [file path-string?]) mc-pair?]{
  Reads an INI file from disk and parses it into an internal mutable cons pair (mc-pair) structure.
  If the file does noet exist, an empty ini structure is made. 
  If file is a symbol?, the file will be constructed from the prefs-dir, the symbol and a suffix ".ini".
  
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
   If file is a symbol?, the file will be constructed from the prefs-dir, the symbol and a suffix ".ini".
  
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
  Sets the value of @racket[key] in the specified @racket[section] of the INI structure. If the section or key does not exist, it is created. Returns the modified INI structure.}


@section{The @racket[ini%] Racket Class}

@defmodule[simple-ini/class]

Require this module for the OO implementation of this Simple INI implementation


@defclass[ini% object% ()]{

An OO wrapper around the ini functions. 

@defconstructor[([file (or/c symbol? string? path? boolean?) #f]
                 [fail (or/c boolean?) #f]
                 )]{
  Creates the ini from the given file.
  * If (eq? file #f), an empty ini will be made.
  * if (symbol? file), an ini will be made or read in the users preferences folder with the given (format "~a.ini" file) as name.
  * Otherwise, the file will be made or read at the given location.

  The fail flag determines if methods of the class will fail when some value in the ini file is written while there is no file to write to
  or if some non existing key is read. 
}

@defmethod*[([(get-file) path?])]{
  Gets the current ini file. See constructor for more information.
}

@defmethod*[([(set-file! [file (or/c symbol? string? path?)]) this])]{
  Sets the ini file to be used. See constructor for more information.
}

@defmethod*[([(get-fail) boolean?])]{
  Gets the value of the 'fail' flag. See constructor for more information.
}

@defmethod*[([(set-fail! [fail boolean?]) this])]{
  Sets the value of the 'fail' flag. See constructor for more information.
}

@defmethod*[([(get-contents) list?])]{
  Gets the contents of the ini structure as stored in memory.
}

@defmethod*[([(contents) list?])]{
  See get-contents.
}

@defmethod*[([(reload) this])]{
  Reloads the ini file in memory, or empties the ini structure (eq? file #f).
}

@defmethod*[([(set! [section (or/c symbol? string?)] [key (or/c symbol? string?)] [value any/c?]) this])]{
  Sets the value of the key in the given section. After the set! operation, the ini structure will be written to file.
  Note. Although ini files can be read from standard .ini formats, the simple-ini format will be enhanced. It wil store values in racket format, so that 'read' can be used to read in the racket value. 
}

@defmethod*[([(get [section (or/c symbol? string?)] [key (or/c symbol? string?)]  [default-value any/c?]) any/c?]
             [(get [section (or/c symbol? string?)] [key (or/c symbol? string)]) any/c]
             )]{
  Returns the value for the given section and key combination. If this combination does not exist in the ini structure, it will return the default-value. However, if default-value is not given, it will return #f.
}

} ; end class
            


@section{The @racket[ini] Roos Class}

@defmodule[simple-ini/roos]{Require this module for the OO implementation of this Simple INI implementation in the ROOS OO framework

Provides a @seclink["top" #:doc '(lib "roos/scribblings/roos.scrbl")]{Roos class} that gives object-oriented access to INI files using the underlying @racket[file->ini] parser system. The class offers methods to load, query, and update INI files using familiar object-style interactions.}

                            
@defproc[(%-! [ini roos-class*] [or/c path-string?]) roos-object*]{
Creates an @racket[ini] object. If a @racket[file] path is provided and the file exists, it is loaded immediately. Otherwise, an empty INI structure is created.

If no file is provided, the object operates in-memory only. Subsequent @racket[set!] operations will raise an error unless a file is later specified with @racket[(file!)].
}


  @defproc[(file) (or/c path-string? #f)]{
    Returns the current filename associated with this INI object.
  }


  @defproc[(file! [f path-string?]) void?]{
    Sets the file to use as backing storage for the INI structure. Triggers a reload from disk.
  }

  @defproc[(fail) boolean?]{
     Returns if an error will be thrown when a set! is done and no file has been set to write the contents to}

  @defproc[(fail! [yn boolean?]) boolean?]{
     Sets fail to the given value.}


  @defproc[(reload) void?]{
    Reloads the INI content from disk, using the current file path.
    If the file does not exist, the content is reset to an empty INI structure.
  }

  @defproc[(set! [section symbol?] [key symbol?] [val any/c]) ini]{
    Sets the value in the INI structure for the given @racket[section] and @racket[key] to @racket[val].

    If a file is associated with the object, the structure is saved to disk immediately.
    If no file is set and @racket[fail] is enabled, an error is raised.
    Returns the INI object itself.
  }

  @defproc[(get [section symbol?] [key symbol?] [def-val any/c] ...) any/c]{
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
  ]

