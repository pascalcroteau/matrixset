
#' EvalScope: Class to handle evaluation environment scope
#'
#' This class manages the environments in which expressions and active bindings
#' are to be evaluated.
#'
#' The environment structure is as follow:
#' ._context_env:
#'   |           environment where the context functions are stored, thus
#'   |           allowing them to be accessible from the apply/mutate functions
#'   |
#'   --- ._elms_env:
#'      |           environment to store temporary elements, created during the
#'      |           evaluation of apply/mutate
#'      |
#'      --- ._enclos_env:
#'                       where the expressions are evaluated. Being a
#'                       (grand-)child of ._context_env/._elms_env, it has
#'                       access to the context functions as well as elements
#'                       created during the evaluation.
#'
#'                       The ._enclos_env environment has two special elements:
#'                       .data and .env. The former contains the same fields and
#'                       active bindings as ._enclos_env do (see, for instance,
#'                       the help of the apply_* functions for the purpose of
#'                       this) while the latter points to the calling
#'                       environment (again, see apply_* for a use case).
#'
#' @note
#' The class has two private variables, `i_` and `j_`, that refer to current
#' row and column indexes to use for sub-setting a matrix prior to function
#' evaluation.
#'
#' But this super class is more general and creates scope environment that are
#' not specific to apply_* evaluation. Therefore, `i_` and `j_` are set to
#' `NULL` and the active binding functions that are defined in this class take
#' that into account.
#'
#' The advantage is that child classes that inherits from `EvalScope` need only
#' to worry about `i_` and `j_` if they need it (e.g., `EvalScopeOfApply`), but
#' can ignore them otherwise (e.g., `EvalScopeOfMutate`).
#'
#' @section Context Functions:
#' The following context functions are defined in the `._context_env`
#' environment:
#'
#' * current_n_row
#' * current_n_column
#' * current_row_name
#' * current_column_name
#' * current_row_info
#' * current_column_info
#' * row_pos
#' * column_pos
#' * row_rel_pos
#' * column_rel_pos
#'
#' Their definition is appropriate for situations that requires neither `i_` nor
#' `j_`, as well as situations where either/both are needed.
#'
#' If more parameters would be needed for a specific case, consider creating a
#' new child class that overloads the context functions.
#'
#' @section Individual Annotations as Active Bindings:
#' Annotations, from both rows and columns, are made available as active
#' bindings.
#'
#' As for the context functions, the bindings are defined appropriately for
#' situations that requires neither `i_` nor `j_`, as well as situations where
#' either/both are needed.
#'
#' If more parameters would be needed for a specific case, consider creating a
#' new child class that overloads the use of annotations.
#'
#' @docType class
#' @noRd
#' @name EvalScope
EvalScope <- R6::R6Class(
  "EvalScope",

  public = list(


    #' Constructor for EvalScope
    #'
    #' @param .ms  The matrix_set object in which to find matrices to apply
    #'             functions to (e.g., as from apply_* or mutate_matrix calls)
    #'             and thus, to which the EvalScope class is assigned to
    #' @param .env The calling environment of the function that needs to use
    #'             EvalScope. This is typically the environment in which apply_*
    #'             or mutate_matrix was called from.
    #'
    initialize = function(.ms, .env) {

      # by setting the calling env as the parent env, this gives access to the
      # bindings of that environment.
      private$._context_env <- new.env(parent = .env)
      private$._elms_env <- new.env(parent = private$._context_env)
      private$._enclos_env <- new.env(parent = private$._elms_env)
      private$._enclos_env$.data <- new.env()
      private$._enclos_env$.env <- .env

      private$._set_context()

      private$._ms <- .ms
      private$._mats <- .subset2(.ms, "matrix_set")
      private$._row_inf <- .subset2(.ms, "row_info")
      private$._col_inf <- .subset2(.ms, "column_info")

      private$._set_info_bindings()
    },



    #' Registers a function to be evaluated.
    #'
    #' @param fn  function to be evaluated. It must be provided in the form of
    #'            an expression.
    #'
    #' @returns
    #' Nothing; used for its side effects.
    register_function = function(fn) {
      private$._fn <- fn
    },



    #' Evaluate a function in the enclosing environment.
    #'
    #' @returns
    #' The outcome of the evaluated functions.
    eval = function() {

      eval(private$._fn, envir = private$._enclos_env)

    }



  ),


  private = list(

    ._context_env = NULL,
    ._elms_env = NULL,
    ._enclos_env = NULL,

    ._ms = NULL,
    ._mats = NULL,

    # integers; for a given matrix, what row (i_) and/or column (j_) subset to
    # use?
    i_ = NULL,
    j_ = NULL,

    ._row_inf = NULL,
    ._col_inf = NULL,

    # function to evaluate
    ._fn = NULL,



    #' Private method
    #'
    #' Assigns context functions to the ._context_env environment.
    #'
    #' Their definition is appropriate for situations that requires neither `i_` nor
    #' `j_`, as well as situations where either/both are needed.
    #'
    #' If more parameters would be needed for a specific case, consider creating
    #' a new child class that overloads the context functions.
    #'
    #' @returns
    #' Nothing; used for its side effects.
    ._set_context = function() {

      private$._context_env$current_n_row <- function() {
        if (is.null(private$i_)) nrow(private$._ms) else length(private$i_)
      }

      private$._context_env$current_n_column <- function() {
        if (is.null(private$j_)) ncol(private$._ms) else length(private$j_)
      }



      private$._context_env$current_row_name <- function() {
        if (is.null(private$i_)) {
          rownames(private$._ms)
        } else {
          rownames(private$._ms)[private$i_]
        }
      }

      private$._context_env$current_column_name <- function() {
        if (is.null(private$j_)) {
          colnames(private$._ms)
        } else {
          colnames(private$._ms)[private$i_]
        }
      }



      private$._context_env$current_row_info <- function() {
        if (is.null(private$i_)) {
          .subset2(private$._ms, "row_info")
        } else {
          .subset2(private$._ms, "row_info")[private$i_, ]
        }
      }

      private$._context_env$current_column_info <- function() {
        if (is.null(private$j_)) {
          .subset2(private$._ms, "column_info")
        } else {
          .subset2(private$._ms, "column_info")[private$j_, ]
        }
      }



      private$._context_env$row_pos <- function() {
        if (is.null(private$i_)) {
          seq_len(nrow(private$._ms))
        } else {
          private$i_
        }
      }

      private$._context_env$column_pos <- function() {
        if (is.null(private$j_)) {
          seq_len(ncol(private$._ms))
        } else {
          private$j_
        }
      }



      private$._context_env$row_rel_pos <- function() {
        cnr <- if (is.null(private$i_)) nrow(private$._ms) else length(private$i_)
        seq_len(cnr)
      }

      private$._context_env$column_rel_pos <- function() {
        cnc <- if (is.null(private$j_)) ncol(private$._ms) else length(private$j_)
        seq_len(cnc)
      }


    },




    #' Private method
    #'
    #' Assigns annotations from rows or columns, as determined by `info`, to the
    #' ._enclos_env environment, as active bindings.
    #'
    #' Their definition is appropriate for situations that requires neither `i_` nor
    #' `j_`, as well as situations where either/both are needed.
    #'
    #' If more parameters would be needed for a specific case, consider creating
    #' a new child class that overloads the context functions.
    #'
    #' @param info one of "row" or "col", determining where to take the
    #'             annotations from
    #'
    #' @returns
    #' Nothing; used for its side effects.
    ._set_bindings_from_inf = function(info) {

      info_name <- paste(".", info, "inf", sep = "_")
      fields <- names(private[[info_name]])

      inf_idx <- if (info == "row") quote(private$i_) else quote(private$j_)

      fn <- function() {}

      for (field in fields) {
        fn_body <- substitute(
          {
            if (is.null(idx)) {
              return(private[[info]][[fld]])
            }
            private[[info]][[fld]][idx]
          },
          list(info = info_name,
               fld = field,
               idx = inf_idx)
        )
        body(fn) <- fn_body

        makeActiveBinding(field, fn, env = private$._enclos_env)
        makeActiveBinding(field, fn, env = private$._enclos_env$.data)
      }

    },



    #' Private method
    #'
    #' Assigns annotations from both rows and columns, by calling
    #' `._set_bindings_from_inf()`.
    #'
    #' @noRd
    #'
    #' @returns
    #' Nothing; used for its side effects.
    ._set_info_bindings = function() {

      private$._set_bindings_from_inf("row")
      private$._set_bindings_from_inf("col")

    }

  )

)
