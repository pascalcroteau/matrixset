




#' LoopStruct: Class to handle apply-implied looping structure
#'
#' This class determines the looping structure behind the apply_* calls. It
#' provides the template of the loops to perform, but is not performing them.
#'
#' The looping structure consists of:
#'
#' * Information on if function(s) are evaluated on each matrix sequentially, or
#'   if the whole matrix set is used as input
#' * Information on when the whole matrix set is the function input, if it is in
#'   list form, or if each matrix is to be accessed individually
#' * The index of of the matrices of the matrix set to use as input (regardless
#'   of _how_ they they are accessed by the function(s) to be evaluated)
#' * Information on margins that are looped upon and why:
#'
#'     - Information on whether looping is even needed. Available globally and
#'       for each margin.
#'     - There is a distinction made on whether a margin loop is due to an
#'       explicit request (e.g., there will be row looping for apply_row) vs.
#'       grouping induced (e.g., there will be row looping involved in
#'       apply_column(row_group_by(object, ...))). This information is handled
#'       by LoopStruct
#'     - The margin indexes to use in a loop iteration.
#'     - In the case of grouping, the grouping basis tibble that contains the
#'       group IDs.
#'     - Which margin is looped on first. Typically it will be row margin, but
#'       it will be column margin if:
#'
#'       - There is column looping but no row looping
#'       - There is column grouping but no row grouping
#'
#'       THe purpose of deciding loop order is for optimization purposes.
#'
#' @docType class
#' @noRd
#' @name LoopStruct
LoopStruct <- R6::R6Class(
  "LoopStruct",

  public = list(


    #' Constructor for LoopStruct
    #'
    #' @param .ms          The matrix_set object to which the apply_* function
    #'                     is applied to and thus, to which the LoopStruct
    #'                     class is assigned to
    #' @param margin       The margin to loop upon. This refers to the apply_*
    #'                     call. A margin of 0 corresponds to apply_matrix, 1
    #'                     to apply_row and 2 to apply_column.
    #' @param mat_subset   matrix indices of which matrix to apply functions
    #'                     to. It can be `NULL` (use all matrices), a `numeric`
    #'                     vector, a `character` vector, or a `logical` vector.
    #'
    #'                     Via a call to ._set_matrix_idx(), the following will
    #'                     happen, based on `mat_subset` type:
    #'
    #'                     *  Numeric values are coerced to integer, the same
    #'                        way `as.integer()` does,  and hence truncation
    #'                        towards zero occurs.
    #'                     *  Negative integers are allowed, indicating
    #'                        elements to leave out.
    #'                     *  Character vectors are matched to the matrix names
    #'                        of the object, thus turned into integers.
    #'                     * logical vectors are _NOT_ recycled, which is an
    #'                       important difference with usual indexing. It means
    #'                       that the `logical` vector must match the number of
    #'                       matrices in length.
    #' @param mat_wise     single logical value, indicating if matrices are
    #'                     looped upon sequentially, or available all at once as
    #'                     input to the functions to evaluate.
    #' @param mats_as_list relevant only if mat_wise is FALSE. Single logical
    #'                     value, indicating if the matrices are provided as a
    #'                     single list (TRUE), or as individual objects.
    #'
    #' @noRd
    initialize = function(.ms, margin, mat_subset, mat_wise, mats_as_list)
    {
      private$matrix_wise_ <- mat_wise
      private$mats_as_list_ <- mats_as_list

      private$matrix_subset_ <- !is.null(mat_subset)
      private$._set_matrix_idx(.ms, mat_subset)
      if (mat_wise) private$._set_matrix_eval_status(.ms)

      row_wise <- margin %.==.% 1
      col_wise <- margin %.==.% 2

      row_grp_meta <- attr(.ms, "row_group_meta")
      col_grp_meta <- attr(.ms, "col_group_meta")

      row_grouped <- !is.null(row_grp_meta) && !row_wise
      col_grouped <- !is.null(col_grp_meta) && !col_wise

      if (!row_grouped) row_grp_meta <- NULL
      if (!col_grouped) col_grp_meta <- NULL

      n_group_row <- if (row_grouped) nrow(row_grp_meta) else NULL
      n_group_col <- if (col_grouped) nrow(col_grp_meta) else NULL

      private$._row_wise <- row_wise
      private$._col_wise <- col_wise
      private$row_group_df_ <- row_grp_meta
      private$col_group_df_ <- col_grp_meta
      private$row_grouped_ <- row_grouped
      private$col_grouped_ <- col_grouped

      private$set_row_groups(.ms)
      private$set_col_groups(.ms)

    }

  ),


  active = list(


    #' @field row_group_df        tibble with a column for each row grouping
    #'                            trait (variable) and a row for each group.
    #'                            This is used to format the results in the
    #'                            context of row grouping.
    #'                            There is also a `.rows` column that stores the
    #'                            group indexes, but it is used as a placeholder
    #'                            for storing the function results
    #' @field col_group_df        tibble with a column for each column grouping
    #'                            trait (variable) and a row for each group.
    #'                            This is used to format the results in the
    #'                            context of column grouping.
    #'                            There is also a `.rows` column that stores the
    #'                            group indexes, but it is used as a placeholder
    #'                            for storing the function results
    #' @field row_grouped         bool that is TRUE if the matrixset object is
    #'                            row-grouped
    #' @field col_grouped         bool that is TRUE if the matrixset object is
    #'                            column-grouped
    #' @field row_groups_for_loop In the context of looping, the row groups are
    #'                            either the rows themselves (e.g., apply_row),
    #'                            or the groups defined by `row_group_by()`.
    #'                            Note that the rows are the groups in the case
    #'                            of apply_row, even if there is a row_group_by
    #'                            in place.
    #'                            `row_groups_for_loop` holds, for each of these
    #'                            groups, the row indexes that correspond to
    #'                            them.
    #' @field col_groups_for_loop In the context of looping, the column groups
    #'                            are either the columns themselves (e.g.,
    #'                            apply_column), or the groups defined by
    #'                            `column_group_by()`.
    #'                            Note that the columns are the groups in the
    #'                            case of apply_column, even if there is a
    #'                            column_group_by in place.
    #'                            `col_groups_for_loop` holds, for each of these
    #'                            groups, the row indexes that correspond to
    #'                            them.
    #'
    #' @field matrix_subsetting   bool that is TRUE if only a subset of the
    #'                            matrices of the matrixset object are to be
    #'                            used.
    #' @field matrix_idx          integer indexes of the matrices to use.
    #' @field matrix_wise         single logical value, indicating if matrices
    #'                            are looped upon sequentially, or available all
    #'                            at once as input to the functions to evaluate.
    #' @field mats_as_list        relevant only if `matrix_wise` is FALSE.
    #'                            Single logical value, indicating if the
    #'                            matrices are provided as a single list (TRUE),
    #'                            or as individual objects.
    #' @field matrix_eval         logical vector, indicating for each matrix to
    #'                            be used if an evaluation is to be performed.
    #'                            The status is TRUE, unless the matrix is
    #'                            `NULL`.
    #'
    #' @field looping             bool that is TRUE if looping - either on rows,
    #'                            columns or both - is required. There is no
    #'                            distinction based on the reason for looping
    #'                            (grouping or per margin evaluation).
    #' @field row_looping         bool that is TRUE if row looping is required.
    #'                            There is no distinction based on the reason
    #'                            for looping (grouping or per row evaluation).
    #' @field col_looping         bool that is TRUE if column looping is
    #'                            required. There is no distinction based on the
    #'                            reason for looping (grouping or per column
    #'                            evaluation).
    #' @field col_looping_first   bool that is TRUE when column needs to happen
    #'                            before row looping. Will be TRUE only if
    #'                            1) There is column looping but no row looping
    #'                            2) There is column grouping but no row grouping

    row_group_df = function() private$row_group_df_,
    col_group_df = function() private$col_group_df_,
    row_grouped = function() private$row_grouped_,
    col_grouped = function() private$col_grouped_,
    row_groups_for_loop = function() private$row_groups_for_loop_,
    col_groups_for_loop = function() private$col_groups_for_loop_,

    matrix_subsetting = function() private$matrix_subset_,
    matrix_idx = function() private$matrix_idx_,
    matrix_wise = function() private$matrix_wise_,
    mats_as_list = function() private$mats_as_list_,
    matrix_eval = function() private$matrix_eval_,

    looping = function() {
      !is.null(private$row_groups_for_loop_) ||
        !is.null(private$col_groups_for_loop_)
    },
    row_looping = function() !is.null(private$row_groups_for_loop_),
    col_looping = function() !is.null(private$col_groups_for_loop_),
    col_looping_first = function() {
      !private$row_grouped_ &&
        (private$col_grouped_ || is.null(private$row_groups_for_loop_))
    }


  ),




  private = list(

    matrix_subset_ = FALSE,
    matrix_idx_ = NULL,
    matrix_eval_ = NULL,
    matrix_wise_ = NULL,
    mats_as_list_ = NULL,
    ._row_wise = NULL,               # TRUE only when row looping was explicitly
                                     # requested and not the result of row
                                     # grouping
    ._col_wise = NULL,               # TRUE only when column looping was
                                     # explicitly requested and not the result
                                     # of column grouping
    row_group_df_ = NULL,
    col_group_df_ = NULL,
    row_grouped_ = NULL,
    col_grouped_ = NULL,
    row_groups_for_loop_ = NULL,
    col_groups_for_loop_ = NULL,



    #' Private method
    #'
    #' Sets the `matrix_idx_` variable (and by extension, the active binding
    #' field `matrix_idx`).
    #'
    #' Note how the elements are named with the matrix names. This is necessary
    #' for latter formatting.
    #'
    #' @param .ms    matrix_set object. Used to obtain some meta info such as
    #'               the number of matrices and their names.
    #' @param matidx matrix indices of which matrix to apply functions to. It
    #'               can be `NULL` (use all matrices), a `numeric` vector, a
    #'               `character` vector, or a `logical` vector.
    #'
    #'               Via a call to index_to_integer(), the following will
    #'               happen, based on `matidx` type:
    #'
    #'               *  Numeric values are coerced to integer, the same
    #'                  way `as.integer()` does, and hence truncation
    #'                  towards zero occurs.
    #'               *  Negative integers are allowed, indicating
    #'                  elements to leave out.
    #'               *  Character vectors are matched to the matrix names
    #'                  of the object, thus turned into integers.
    #'               *  logical vectors are _NOT_ recycled, which is an
    #'                  important difference with usual indexing. It means
    #'                  that the `logical` vector must match the number of
    #'                  matrices in length.
    #'
    #' @returns
    #' Nothing; used for its side effects.
    ._set_matrix_idx = function(.ms, matidx)
    {
      if (is.null(matidx)) {
        nmat <- .nmatrix(.ms)
        matidx <- seq_len(nmat)
        matnms <- matrixnames(.ms)
      } else {
        matnms <- .matrixnames(.ms)
        matidx <- index_to_integer(matidx, .nmatrix(.ms), matnms)
        matnms <- matnms[matidx]
      }

      matidx <- stats::setNames(matidx, matnms)
      private$matrix_idx_ <- matidx

    },



    #' Private method
    #'
    #' Sets the `matrix_eval_` variable (and by extension, the active binding
    #' field `matrix_eval`).
    #'
    #' @param .ms matrix_set object. Used to determine for each matrix if it is
    #'            `NULL` or not.
    #'
    #' @returns
    #' Nothing; used for its side effects.
    ._set_matrix_eval_status = function(.ms)
    {
      mats <- .subset2(.ms, "matrix_set")
      mat_eval <- !vapply(mats, is.null, FALSE)
      private$matrix_eval_ <- mat_eval[private$matrix_idx_]
    },




    #' Private method
    #'
    #' Sets the `row_groups_for_loop_` variable (and by extension, the active
    #' binding field `row_groups_for_loop`).
    #'
    #' @param .ms matrix_set object. Used to determine the number of rows in the
    #'            case where functions are to be evaluated for each row, and
    #'            thus row looping is not the result of row grouping.
    #'
    #' @returns
    #' Nothing; used for its side effects.
    set_row_groups = function(.ms) {

      if (private$._row_wise) {

        row_grs <- as.list(seq_len(nrow(.ms)))
        names(row_grs) <- rownames(.ms)
        private$row_groups_for_loop_ <- row_grs

      } else if (private$row_grouped_) {

        private$row_groups_for_loop_ <- private$row_group_df_$.rows

      }

    },




    #' Private method
    #'
    #' Sets the `col_groups_for_loop_` variable (and by extension, the active
    #' binding field `col_groups_for_loop`).
    #'
    #' @param .ms matrix_set object. Used to determine the number of columns in
    #'            the case where functions are to be evaluated for each column,
    #'            and thus column looping is not the result of column grouping.
    #'
    #' @noRd
    #'
    #' @returns
    #' Nothing; used for its side effects.
    set_col_groups = function(.ms) {

      if (private$._col_wise) {

        col_grs <- as.list(seq_len(ncol(.ms)))
        names(col_grs) <- colnames(.ms)
        private$col_groups_for_loop_ <- col_grs

      } else if (private$col_grouped_) {

        private$col_groups_for_loop_ <- private$col_group_df_$.rows

      }

    }

  )

)






#' EvalScopeOfApply: Class to handle evaluation environment scope specific to
#' apply_* functions.
#'
#' It is a child of EvalScope that makes use of the row/column sub-setting
#' mechanism, to which it adds the following functionalities:
#'
#' * Has a mechanism for matrix selection/sub-setting
#' * Uses a LoopStruct object to properly subset the matrices when needed
#' * Provides a way to update `i_`, `j_` and `k_` (exclusive to EvalScopeOfApply)
#' * Defines the apply_\* pronouns as active bindings. These pronouns are
#'   typically `.i`, `.j` or `.m`, but they are defined by `var_lab_row`,
#'   `var_lab_col` and `var_lab_mat`.
#'
#'
#' @docType class
#' @noRd
#' @name EvalScopeOfApply
EvalScopeOfApply <- R6::R6Class(
  "EvalScopeOfApply",

  inherit = EvalScope,

  public = list(


    #' Constructor for EvalScopeOfApply
    #'
    #' @param .ms         The matrix_set object in which to find matrices to
    #'                    apply functions to and thus, to which the
    #'                    EvalScopeOfApply class is assigned to
    #' @param margin      0, 1 or 2. 0 = `apply_matrix` has been called.
    #'                    1 = `apply_row` and 2 = `apply_column`. The dfw/dfl
    #'                    versions are covered as well.
    #' @param loop_struct an instance of a LoopStruct object.
    #' @param .env        The calling environment of the function that needs to
    #'                    use EvalScopeOfApply This is typically the environment
    #'                    in which apply_* was called from.
    #'
    initialize = function(.ms, margin, loop_struct, .env) {

      super$initialize(.ms, .env)

      private$._margin <- margin
      private$._set_pronoun(margin)
      private$._loop_struct <- loop_struct

      private$._set_bindings()

    }


  ),


  active = list(

    #' Setters
    #'
    #' @field i, j, k    Active bindings to set the private fields `i_`, `j_`
    #'                   and `k_`. This allows to evaluate functions on the
    #'                   proper matrix subsets.
    i = function(new_i) private$i_ <- new_i,
    j = function(new_j) private$j_ <- new_j,
    k = function(new_k) private$k_ <- new_k

  ),


  private = list(

    # integer; which matrix or matrices to use
    k_ = NULL,

    ._margin = NULL,          # private holder of `margin`
    ._pronoun = NULL,         # available pronoun, typically one of `.i`, `.j`
                              # or `.m`, but they are defined by `var_lab_row`,
                              # `var_lab_col` and `var_lab_mat`

    ._loop_struct = NULL,     # an instance of a LoopStruct object, which stores
                              # various necessary instructions on how to
                              # loop/group


    # Private expressions
    #
    # These are used to build active binding functions.
    #
    # The first batch corresponds to matrices (all, as a list, a single one, or
    # a subset in list format)
    #
    # The second batch builds on the same concept but provides row/column
    # subsetting flavors.
    #
    # Pseudo variables ._k_ (represents matrix index) and ._M_ (represents a
    # matrix) are placeholders to be replaced when building the active binding
    # functions. See ._generate_mat_subset_expr() and
    # ._generate_mat_subset_within_loop_expr() below.
    #
    # Note: The use of ._M_[private$i_, ][private$j_] or
    #       ._M_[, private$j_][private$i_], as opposed to
    #       ._M_[private$i_, private$j_], is to keep rownames/colnames at all
    #       times, something the construction ._M_[private$i_, private$j_]
    #       doesn't guarantee.

    ._mat_whole = quote(private$._mats),
    ._mat_subset_k = quote(private$._mats[[._k_]]),
    ._mat_subset_k_lst = quote(private$._mats[._k_]),

    ._M_subset_i_drop = quote(._M_[private$i_, ]),
    ._M_subset_i_no_drop = quote(._M_[private$i_, , drop = FALSE]),
    ._M_subset_i_then_j = quote(._M_[private$i_, ][private$j_]),
    ._M_subset_j_drop = quote(._M_[, private$j_]),
    ._M_subset_j_no_drop = quote(._M_[, private$j_, drop = FALSE]),
    ._M_subset_j_then_i = quote(._M_[, private$j_][private$i_]),
    ._M_subset_ij = quote(._M_[private$i_, private$j_, drop = FALSE]),




    #' Private method
    #'
    #' Sets the private$._pronoun variable, which is the pronoun available to
    #' use in the apply* function.
    #'
    #' @param margin    One of 0 (no margin; whole matrix), 1 (row) or 2 (column)
    #'
    #' @returns
    #' Nothing; used for its side effects.
    ._set_pronoun = function(margin) {

      private$._pronoun <- get_var_tag(margin)

    },



    #' Private method
    #'
    #' Generates an expression to be used for building active binding functions.
    #' The expression is for cases where a single matrix is evaluated at a time.
    #'
    #' @param k_expr   an expression, what to replace ._k_ with; an expression
    #'                 that represents matrix index
    #' @param sub_expr an expression, representing what row and/or column subset
    #'                 of the matrix is of interest
    #'
    #' @returns
    #' an expression
    #'
    #' @examples
    #' private$._generate_mat_subset_expr(quote(k), private$._M_subset_i_drop)
    #' # gives: private$._mats[[k]][private$i_, ]
    #'
    ._generate_mat_subset_expr = function(k_expr, sub_expr) {

      do.call("substitute",
              list(sub_expr,
                   list(._M_ = do.call(substitute,
                                       list(private$._mat_subset_k,
                                            list(._k_ = k_expr))
                   )
                   )
              )
      )
    },




    #' Private method
    #'
    #' Generates an expression to be used for building active binding functions.
    #' The expression is for cases where the function to be evaluated has access
    #' to all matrices at once.
    #'
    #' @param seq_expr an expression, what to replace ._k_ with; an expression
    #'                 that represents matrix index
    #' @param sub_expr an expression, representing what row and/or column subset
    #'                 of the matrix is of interest
    #'
    #' @returns
    #' an expression
    #'
    #' @examples
    #' private$._generate_mat_subset_within_loop_expr(private$._mat_subset_k_lst,
    #'                                                private$._M_subset_i_drop)
    #' # gives: lapply(private$._mats[private$k_], function(m) m[private$i_, ])
    ._generate_mat_subset_within_loop_expr = function(seq_expr, sub_expr) {
      do.call(substitute,
              list(substitute(lapply(._ms_, function(m) ._m_expr_),
                              list(._ms_ = do.call(substitute,
                                                   list(seq_expr,
                                                        list(._k_ = quote(private$k_)))
                              ),
                              ._m_expr_ = sub_expr)),
                   list(._M_ = quote(m))))
    },




    #' Private method
    #'
    #' Generates an expression to be used for building active binding functions.
    #'
    #' The expression is for cases where a single matrix is evaluated at a time,
    #' from a call to `apply_row` (or dfw/dfl variant). It deals with cases
    #' `.matrix_wise = TRUE` or  `.matrix_wise = FALSE` and `.input_list = FALSE`.
    #' For `.matrix_wise = FALSE` and `.input_list = TRUE`, see
    #' ._generate_row_subset_multi().
    #'
    #' If a column subset is also taken, then the row subset is taken first.
    #'
    #'
    #' @param k    an integer or an expression, what to replace ._k_ with; the
    #'             matrix index, or an expression that represents matrix index
    #'
    #' @returns
    #' an expression
    ._generate_row_subset = function(k) {
      if (private$._loop_struct$col_looping)
        return(private$._generate_mat_subset_expr(k,
                                                  private$._M_subset_i_then_j))

      private$._generate_mat_subset_expr(k, private$._M_subset_i_drop)
    },





    #' Private method
    #'
    #' Generates an expression to be used for building active binding functions.
    #'
    #' The expression is for cases where a list of matrix is evaluated, from a
    #' call to `apply_row` (or dfw/dfl variant), with options
    #' `.matrix_wise = FALSE` and `.input_list = TRUE`.
    #'
    #' If a column subset is also taken, then the row subset is taken first.
    #'
    #' @returns
    #' an expression
    ._generate_row_subset_multi = function() {

      if (private$._loop_struct$matrix_subsetting) {

        if (private$._loop_struct$col_looping)
          return(
            private$._generate_mat_subset_within_loop_expr(private$._mat_subset_k_lst,
                                                           private$._M_subset_i_then_j)
          )

        return(
          private$._generate_mat_subset_within_loop_expr(private$._mat_subset_k_lst,
                                                         private$._M_subset_i_drop)
        )
      }

      if (private$._loop_struct$col_looping)
        return(
          private$._generate_mat_subset_within_loop_expr(private$._mat_whole,
                                                         private$._M_subset_i_then_j)
        )

      private$._generate_mat_subset_within_loop_expr(private$._mat_whole,
                                                     private$._M_subset_i_drop)

    },





    #' Private method
    #'
    #' Generates an expression to be used for building active binding functions.
    #'
    #' The expression is for cases where a single matrix is evaluated at a time,
    #' from a call to `apply_column` (or dfw/dfl variant). It deals with cases
    #' `.matrix_wise = TRUE` or  `.matrix_wise = FALSE` and `.input_list = FALSE`.
    #' For `.matrix_wise = FALSE` and `.input_list = TRUE`, see
    #' ._generate_col_subset_multi().
    #'
    #' If a row subset is also taken, then the column subset is taken first.
    #'
    #' @param k    an integer or an expression, what to replace ._k_ with; the
    #'             matrix index, or an expression that represents matrix index
    #'
    #' @returns
    #' an expression
    ._generate_col_subset = function(k) {
      if (private$._loop_struct$row_looping)
        return(private$._generate_mat_subset_expr(k,
                                                  private$._M_subset_j_then_i))

      private$._generate_mat_subset_expr(k, private$._M_subset_j_drop)
    },




    #' Private method
    #'
    #' Generates an expression to be used for building active binding functions.
    #'
    #' The expression is for cases where a list of matrix is evaluated, from a
    #' call to `apply_column` (or dfw/dfl variant), with options
    #' `.matrix_wise = FALSE` and `.input_list = TRUE`.
    #'
    #' If a row subset is also taken, then the column subset is taken first.
    #'
    #' @returns
    #' an expression
    ._generate_col_subset_multi = function() {

      if (private$._loop_struct$matrix_subsetting) {

        if (private$._loop_struct$row_looping)
          return(
            private$._generate_mat_subset_within_loop_expr(private$._mat_subset_k_lst,
                                                           private$._M_subset_j_then_i)
          )

        return(
          private$._generate_mat_subset_within_loop_expr(private$._mat_subset_k_lst,
                                                         private$._M_subset_j_drop)
        )
      }

      if (private$._loop_struct$row_looping)
        return(
          private$._generate_mat_subset_within_loop_expr(private$._mat_whole,
                                                         private$._M_subset_j_then_i)
        )

      private$._generate_mat_subset_within_loop_expr(private$._mat_whole,
                                                     private$._M_subset_j_drop)

    },




    #' Private method
    #'
    #' Generates an expression to be used for building active binding functions.
    #'
    #' The expression is for cases where a single matrix is evaluated at a time,
    #' from a call to `apply_matrix` (or dfw/dfl variant). It deals with cases
    #' `.matrix_wise = TRUE` or  `.matrix_wise = FALSE` and `.input_list = FALSE`.
    #' For `.matrix_wise = FALSE` and `.input_list = TRUE`, see
    #' ._generate_mat_subset_multi().
    #'
    #' @param k    an integer or an expression, what to replace ._k_ with; the
    #'             matrix index, or an expression that represents matrix index
    #'
    #' @returns
    #' an expression
    ._generate_mat_subset = function(k) {

      if (private$._loop_struct$row_looping) {

        if (private$._loop_struct$col_looping)
          return(private$._generate_mat_subset_expr(k,
                                                    private$._M_subset_ij))

        return(private$._generate_mat_subset_expr(k,
                                                  private$._M_subset_i_no_drop))
      }


      if (private$._loop_struct$col_looping)
        return(private$._generate_mat_subset_expr(k,
                                                  private$._M_subset_j_no_drop))

      do.call(substitute, list(private$._mat_subset_k,
                               list(._k_ = k)))
    },




    #' Private method
    #'
    #' Generates an expression to be used for building active binding functions.
    #'
    #' The expression is for cases where a list of matrix is evaluated, from a
    #' call to `apply_matrix` (or dfw/dfl variant), with options
    #' `.matrix_wise = FALSE` and `.input_list = TRUE`.
    #'
    #' @noRd
    #'
    #' @returns
    #' an expression
    ._generate_mat_subset_multi = function() {

      if (private$._loop_struct$matrix_subsetting) {

        if (private$._loop_struct$row_looping) {

          if (private$._loop_struct$col_looping)
            return(private$._mat_subset_multi_with_k_with_i_with_j_expr)

          return(private$._mat_subset_multi_with_k_with_i_no_j_expr)

        }

        if (private$._loop_struct$col_looping)
          return(private$._mat_subset_multi_with_k_no_i_with_j_expr)

        return(private$._mat_subset_multi_with_k_no_i_no_j_expr)

      }


      if (private$._loop_struct$row_looping) {

        if (private$._loop_struct$col_looping)
          return(private$._mat_subset_multi_no_k_with_i_with_j_expr)

        return(private$._mat_subset_multi_no_k_with_i_no_j_expr)

      }

      if (private$._loop_struct$col_looping)
        return(private$._mat_subset_multi_no_k_no_i_with_j_expr)

      return(private$._mat_subset_multi_no_k_no_i_no_j_expr)

    },




    #' Private method
    #'
    #' Generates an active binding.
    #'
    #' The expression is for cases where a single matrix is evaluated at a time,
    #' from a call to `apply_matrix` (or dfw/dfl variant). It deals with cases
    #' `.matrix_wise = TRUE` or  `.matrix_wise = FALSE` and `.input_list = FALSE`.
    #' For `.matrix_wise = FALSE` and `.input_list = TRUE`, see
    #' ._generate_mat_subset_multi().
    #'
    #' @param name       string, the name of the active binding
    #' @param fn_body    expression, the body function of the active binding
    #' @param in_data    bool. When TRUE, the binding is also created in the
    #'                   .data environment
    #'
    #' @returns
    #' Nothing; used for its side effects.
    ._make_binding = function(name, fn_body, in_data = FALSE) {

      fn <- function() {}
      body(fn) <- fn_body

      makeActiveBinding(name, fn, env = private$._enclos_env)
      if (in_data) makeActiveBinding(name, fn, env = private$._enclos_env$.data)

    },



    #' Private method
    #'
    #' Generates the appropriate apply_* pronoun as an active binding.
    #'
    #' This is for the case where matrices are evaluated individually in turn.
    #'
    #' Also sets a custom message for trying to use an unavailable binding.
    #'
    #' @returns
    #' Nothing; used for its side effects.
    ._set_bindings_from_single_mat = function() {

      active_name <- private$._pronoun
      not_active_name <- setdiff(c(var_lab_row, var_lab_col, var_lab_mat),
                                 active_name)


      fn_body <- if (private$._margin == 1) {
        private$._generate_row_subset(quote(private$k_))
      } else if (private$._margin == 2) {
        private$._generate_col_subset(quote(private$k_))
      } else {
        private$._generate_mat_subset(quote(private$k_))
      }

      private$._make_binding(active_name, fn_body)

      for (v in not_active_name) {
        msg <- glue::glue("object {OBJ} not found", OBJ = v)
        fn_body <- substitute(stop(MSG, call. = FALSE), list(MSG=msg))
        private$._make_binding(v, fn_body)
      }
    },



    #' Private method
    #'
    #' Generates the appropriate apply_* pronoun as an active binding.
    #'
    #' This is for the case where matrices are all available for evaluation at
    #' once, individually.
    #'
    #' @returns
    #' Nothing; used for its side effects.
    #'
    #' TODO:
    #' Also sets a custom message for trying to use an unavailable binding.
    ._set_bindings_from_multi_mat_sep = function() {

      active_name <- private$._pronoun

      not_active_name <- setdiff(c(var_lab_row, var_lab_col, var_lab_mat),
                                 active_name)


      idx <- if (private$._loop_struct$matrix_subsetting) {
        private$._loop_struct$matrix_idx
      } else {
        seq_along(private$._mats)
      }
      fields <- private$._mat_names[idx]



      for (mi in seq_along(idx)) {

        field <- paste0(active_name, mi)

        fn_body <- if (private$._margin == 1) {
          private$._generate_row_subset(idx[mi])
        } else if (private$._margin == 2) {
          private$._generate_col_subset(idx[mi])
        } else {
          private$._generate_mat_subset(idx[mi])
        }

        private$._make_binding(field, fn_body, in_data = TRUE)

        # for (v in not_active_name) {
        #   msg <- glue::glue("object {OBJ} not found", OBJ = v)
        #   fn <- function() {}
        #   fn_body <- substitute(stop(MSG, call. = FALSE), list(MSG=msg))
        #   body(fn) <- fn_body
        #   makeActiveBinding(v, fn, env = private$._enclos_env)
        # }
      }
    },



    #' Private method
    #'
    #' Generates the appropriate apply_* pronoun as an active binding.
    #'
    #' This is for the case where matrices are all available for evaluation at
    #' once. If not as a list, it defers to ._set_bindings_from_multi_mat_sep().
    #'
    #' @returns
    #' Nothing; used for its side effects.
    #'
    #' TODO:
    #' Also sets a custom message for trying to use an unavailable binding.
    ._set_bindings_from_multi_mat = function() {

      active_name <- private$._pronoun
      not_active_name <- setdiff(c(var_lab_row, var_lab_col, var_lab_mat),
                                 active_name)

      if (!private$._loop_struct$mats_as_list) {
        return(private$._set_bindings_from_multi_mat_sep())
      }


      fn_body <- if (private$._margin == 1) {
        private$._generate_row_subset_multi()
      } else if (private$._margin == 2) {
        private$._generate_col_subset_multi()
      } else {
        private$._generate_mat_subset_multi()
      }

      private$._make_binding(active_name, fn_body)

      # for (v in not_active_name) {
      #   msg <- glue::glue("object {OBJ} not found", OBJ = v)
      #   fn <- function() {}
      #   fn_body <- substitute(stop(MSG, call. = FALSE), list(MSG=msg))
      #   body(fn) <- fn_body
      #   makeActiveBinding(v, fn, env = private$._enclos_env)
      # }
    },



    #' Private method
    #'
    #' Generates the appropriate apply_* pronoun as an active binding.

    #' TODO:
    #' Also sets a custom message for trying to use an unavailable binding.
    ._set_bindings = function() {

      if (!private$._loop_struct$matrix_wise) {

        private$._set_bindings_from_multi_mat()
        return()

      }

      private$._set_bindings_from_single_mat()

    }

  )

)










#' Applyer: applies function(s) to the matrices of a matrix_set object.
#'
#' Performs the loops as dictated by the loop template - determined by
#' LoopStruct - and evaluate the functions.
#'
#' Formatting is applied as well, based on different parameters:
#'
#' * Simplification always yields a tibble output - per matrix when
#'   `.matrix_wise` is TRUE. Simplification is triggered by the use of the
#'   dfw/dfl variants of the apply functions.
#'     * Information columns are created if long format has been selected.
#'     * Proper column names are created if wide format has been selected.
#' * In presence of relevant grouping, a tibble is always returned - per matrix
#'   when `.matrix_wise` is TRUE. Without simplification, there is a tibble row
#'   per group and a column per grouping variable. With simplification, the
#'   columns are also provided.
#'
#' @docType class
#' @noRd
#' @name Applyer
Applyer <- R6::R6Class(
  "Applyer",

  public = list(


    #' Constructor for EvalScopeOfApply
    #'
    #' @param .ms         The matrix_set object in which to find matrices to
    #'                    apply functions to and thus, to which the
    #'                    Applyer class is assigned to
    #' @param matidx      `NULL`, which means all matrices are used, or the
    #'                    matrices to use, provided as:
    #'
    #'                    * Numeric values, coerced to integer through
    #'                      [as.integer()] if not already integers. This means
    #'                      potential truncation towards zero.
    #'                      Negative values are allowed, interpreded as matrices
    #'                      to leave out.
    #'                    * Character vector, matched to the matrix names.
    #'                    * Logival vector, stating for each element if it is
    #'                      used (`TRUE`) or discarded (`FALSE`). Logical
    #'                      vectors are *NOT* recycled, which means that the
    #'                      `logical` vector must match the object dimension in
    #'                      length.
    #'                    Simply passed to LoopStruct which builds the looping
    #'                    structure template.
    #' @param margin      0, 1 or 2. 0 = `apply_matrix` has been called.
    #'                    1 = `apply_row` and 2 = `apply_column`. The dfw/dfl
    #'                    versions are covered as well.
    #' @param fns         list of functions to evaluate. Each function must be
    #'                    provided as an expression that can evaluated through
    #'                    `eval`.
    #' @param mat_wise    single logical value, indicating if matrices are
    #'                    looped upon sequentially, or available all at once as
    #'                    input to the functions to evaluate.
    #'                    Simply passed to LoopStruct which builds the looping
    #'                    structure template.
    #' @param as_list     relevant only if mat_wise is FALSE. Single logical
    #'                    value, indicating if the matrices are provided as a
    #'                    single list (TRUE), or as individual objects.
    #'                    Simply passed to LoopStruct which builds the looping
    #'                    structure template.
    #' @param simplify    single character, one of "no", "long" or "wide".
    #'                    Refers to output format, relevant only if the output
    #'                    is a vector of length > 1. In this case, long format
    #'                    has a row per outcome, identified by an additional
    #'                    column, while they are separate columns in wide format.
    #' @param force_name  bool, used only for the simplified output versions
    #'                    (dfl/dfw). If FALSE, function IDs will be provided
    #'                    only if the function outcome is a vector of length 2
    #'                    or more. If `force_name` is `TRUE` then function IDs
    #'                    are provided in all situations.
    #'
    #'                    This can be useful in situation of grouping. As the
    #'                    functions are evaluated independently within each
    #'                    group, there could be situations where function
    #'                    outcomes are of length 1 for some groups and lenght 2
    #'                    or more in other groups.
    #' @param env         The calling environment of the function that needs to
    #'                    use EvalScopeOfApply This is typically the environment
    #'                    in which apply_* was called from.
    #'
    initialize = function(.ms, matidx, margin, fns, mat_wise, as_list, simplify,
                          force_name, env) {

      # looping structure template
      private$._loop_struct <- LoopStruct$new(.ms, margin, matidx, mat_wise,
                                              as_list)

      private$._margin <- margin
      private$._set_matrix_meta(.ms)

      private$._tag <- switch(margin,
                              "1" = .rowtag(.ms),
                              "2" = .coltag(.ms),
                              NULL)

      private$._simplify <- simplify
      private$._force_name <- force_name

      private$._set_fn_meta(fns)
      private$._set_long_col_order()


      # where to eval the functions; pronouns are assigned too
      private$._scope <- EvalScopeOfApply$new(.ms, margin, private$._loop_struct, env)
    },


    #' Evaluate the functions
    #'
    #' @returns
    #' The outcome of the evaluated functions.
    #'
    #' A list for every matrix in the matrixset object. Each list is itself a
    #' list, or `NULL` for `NULL` matrices. For `apply_matrix`, it is a list of
    #' the function values. Otherwise, it is a list with one element for each
    #' row/column. And finally, for `apply_row`/`apply_column`, each of these
    #' sub-list is a list, the results of each function.
    #'
    #' When `.matrix_wise == FALSE`, the output format differs only in that
    #' there is no list for matrices.
    #'
    #' For the dfl/dfw variants, if each function returns a `vector` of the same
    #' dimension, returns a list of `tibble`s. The `dfl` version will stack the
    #' function results in a long format while the `dfw` version will put them
    #' side-by-side, in a wide format.
    #'
    #' If the functions returned vectors of more than one element, there will be
    #' a column to store the values and one for the function ID (dfl), or one
    #' column per combination of function/result (dfw)
    #'
    #' In grouping context, the output format is different. A list for every
    #' matrix is still returned, but each of these lists now holds a tibble.
    #'
    #' Each tibble has a column called `.vals`, where the function results are
    #' stored. This column is a list, one element per group. The group labels are
    #' given by the other columns of the tibble. For a given group, things are like
    #' the ungrouped version: further sub-lists for rows/columns - if applicable -
    #' and function values.
    #'
    #' The dfl/dfw versions are more similar in their output format to their
    #' ungrouped version. The format is almost identical, except that additional
    #' columns are reported to identify the group labels..
    eval = function() private$eval_()



  ),


  private = list(

    ._scope = NULL,                  # an instance of class EvalScopeOfApply

    ._margin = NULL,                 # margin provided as input to functions

    ._mat_n = NULL,                  # number of matrices to use
    ._mat_names = NULL,              # names of matrices to use

    ._tag = NULL,                    # tag to refer to rownames/colnames

    ._loop_struct = NULL,            # an instance of class LoopStruct

    ._fns = list(function(x) x),     # list of functions to evaluate.
    ._fns_n = 0,                     # number of functions to evaluate

    ._fns_outcome = NULL,            # vector of raw function outcomes
    ._fns_outcome_formatted = NULL,  # vector of formatted function outcomes
    ._fns_outcome_names = NULL,      # names associated to each function outcome
    ._fn_out_lens = NULL,            # length of each function outcome

    ._mat_outcome = NULL,            # stores function outcomes, on a matrix level
    ._row_outcome = NULL,            # stores function outcomes, on a row level
    ._col_outcome = NULL,            # stores function outcomes, on a column level

    ._long_order = NULL,             # order of the columns in 'long' format
                                     # simplification.

    ._simplify = "no",               # "no", "wide" or "long", the
                                     # simplification status
    ._force_name = FALSE,            # bool; Are function IDs provided in all
                                     # situations?




    #' Private method
    #'
    #' Initialize private fields that relates to matrix info:
    #'
    #' * number of matrices, accounting for those to ignore
    #' * matrix names
    #' * ._mat_oucome, the list that will store the evaluation outcome per
    #'   matrix
    #'
    #' @param .ms    matrix_set object, used to extract matrix names.
    #'
    #' @returns
    #' nothing; used for its side effects.
    ._set_matrix_meta = function(.ms)
    {
      matidx <- private$._loop_struct$matrix_idx
      nmat <- length(matidx)

      matnms <- .matrixnames(.ms)
      matnms <- matnms[matidx]

      private$._mat_n <- nmat
      private$._mat_names <- matnms

      if (private$._loop_struct$matrix_wise && private$._mat_n > 0L) {
        private$._mat_outcome <- vector('list', private$._mat_n)
        names(private$._mat_outcome) <- private$._mat_names
      }

    },




    #' Private method
    #'
    #' Sets private field `._fns_outcome_names` (the names associated to each
    #' function outcome) to an empty vector.
    #'
    #' Serves both at initializing the vector and resetting the vector,
    #' something necessary when iterating from a group to another.
    #'
    #' This applies only in the context of outcome simplification.
    #'
    #' @returns
    #' nothing; used for its side effects.
    ._reset_fns_outcome_names = function() {

      if (private$._simplify != "no")
        private$._fns_outcome_names <- vector('list', private$._fns_n)

    },



    #' Private method
    #'
    #' Sets private fields that relates to the functions to be evaluated.
    #'
    #' - ._fns
    #' - ._fns_n
    #' - ._fns_outcome
    #' - ._fn_out_lens (if applicable)
    #' - ._fns_outcome_names (if applicable, via ._reset_fns_outcome_names())
    #' - ._long_order (if applicable)
    #'
    #'
    #' Serves both at initializing the vector and resetting the vector,
    #' something necessary when iterating from a group to another.
    #'
    #' This applies only in the context of outcome simplification.
    #'
    #' @param fns    the functions, provided as elements of a list
    #'
    #' @returns
    #' nothing; used for its side effects.
    ._set_fn_meta = function(fns) {

      private$._fns <- fns
      private$._fns_n <- length(fns)

      private$._fns_outcome <- vector('list', length(fns))
      names(private$._fns_outcome) <- names(fns)

      if (private$._simplify != "no")
        private$._fn_out_lens <- integer(length(fns))

      private$._reset_fns_outcome_names()

    },


    #' Private method
    #'
    #' order of the columns in 'long' format simplification. It makes sure the
    #' column with the names are just before their respective outcome column,
    #' and it keeps the original function order.
    #'
    #' For a given n, it generates a vector of length 2n: 1 (n+1) 2 (n+2)... 2*n
    #'
    #' @returns
    #' nothing; used for its side effects.
    #'
    #' @examples
    #' For n = 5, it generates the vector 1  6  2  7  3  8  4  9  5 10
    ._set_long_col_order = function() {

      if (private$._simplify == "long") {
        n <- private$._fns_n
        rder <- as.vector(rbind(1:n, n+(1:n)))
        private$._long_order <- rder
      }

    },




    #' Private method
    #'
    #' Sets private field `._row_outcome` (the vector that stores outcome on a
    #' row level).
    #'
    #' Serves both at initializing the vector and resetting the vector,
    #' something necessary when row groups is an inner loop to the column outer
    #' loop, where a reset is necessary.
    #'
    #' @returns
    #' nothing; used for its side effects.
    ._reset_row_outcome = function() {

      private$._row_outcome <- vector('list',
                                      length(private$._loop_struct$row_groups_for_loop))
      names(private$._row_outcome) <- names(private$._loop_struct$row_groups_for_loop)

    },



    #' Private method
    #'
    #' Sets private field `._col_outcome` (the vector that stores outcome on a
    #' column level).
    #'
    #' Serves both at initializing the vector and resetting the vector,
    #' something necessary when column groups is an inner loop to the row outer
    #' loop, where a reset is necessary.
    #'
    #' @returns
    #' nothing; used for its side effects.
    ._reset_col_outcome = function() {

      private$._col_outcome <- vector('list',
                                      length(private$._loop_struct$col_groups_for_loop))
      names(private$._col_outcome) <- names(private$._loop_struct$col_groups_for_loop)

    },





    #' Private method
    #'
    #' Execute the function evaluation process and returns the formatted
    #' outcome.
    #'
    #' This function "subcontracts" to the appropriate process, based on the
    #' value of matrix_wise.
    #'
    #' @returns
    #' The formatted outcome of the evaluated functions. See `eval` of this very
    #' `Applyer` class for details of the format.
    eval_ = function() {

      if (private$._mat_n == 0L) return(invisible())

      if (!private$._loop_struct$matrix_wise) {
        return(private$._eval_multi())
      }

      private$._eval_by_matrix()
      private$._mat_outcome
    },


    #' Private method
    #'
    #' Execute the function evaluation process and creates the formatted
    #' outcome.
    #'
    #' This function is a subcontractor and handles the loops across matrices.
    #'
    #' Based on the looping template structure, it will also perform the loops
    #' across rows and/or across columns (in the order that is the most
    #' efficient), by subcontracting the job to ._eval_by_row_groups() or
    #' ._eval_by_col_groups().
    #'
    #' Note: The first loop (after the matrix loop) is almost always loop on
    #' row. Exception only if there is column grouping - but not row grouping
    #' (or not rowwise). But this is handled by loop_struct and ._eval_by_matrix
    #' only needs to know what to call first, not why.
    #'
    #' @returns
    #' nothing; used for its side effects.
    ._eval_by_matrix = function() {

      for (midx in seq_len(private$._mat_n)) {

        if (!private$._loop_struct$matrix_eval[midx]) {
          private$._mat_outcome[midx] <- list(NULL)
          next
        }


        private$._scope$k <- private$._loop_struct$matrix_idx[midx]

        if (!private$._loop_struct$looping) {

          private$._eval_fns()
          private$._mat_outcome[[midx]] <- private$._format_list_of_evals(private$._fns_outcome_formatted)
          next
        }


        if (private$._loop_struct$col_looping_first) {

          private$._eval_by_col_groups(inner = FALSE)
          private$._mat_outcome[[midx]] <- private$._col_outcome
          next

        }

        private$._eval_by_row_groups(inner = FALSE)
        private$._mat_outcome[[midx]] <- private$._row_outcome


        if (private$._margin == 0 && private$._loop_struct$row_grouped &&
            private$._loop_struct$col_grouped && private$._simplify == "no") {

          private$._mat_outcome[[midx]] <- tidyr::unnest(private$._mat_outcome[[midx]],
                                                         cols = .vals)
        }

      }



    },





    #' Private method
    #'
    #' Execute the function evaluation process and creates the formatted
    #' outcome.
    #'
    #' This function performs similar tasks to ._eval_by_matrix(), but does not
    #' loop across matrices as it handles the calls (apply_matrix, apply_row,
    #' apply_column and dfw/dfl variants) with .matrix_wise = FALSE, i.e., all
    #' matrices are available for function input at once.
    #'
    #' This function is a subcontractor and handles loops across rows and
    #' columns, if applicable, by subcontracting to ._eval_by_row_groups() and
    #' ._eval_by_col_groups().
    #'
    #' Note: The first loop is almost always loop on row. Exception only if
    #' there is column grouping - but not row grouping (or not rowwise). But
    #' this is handled by loop_struct and ._eval_by_matrix only needs to know
    #' what to call first, not why.
    #'
    #' @returns
    #' nothing; used for its side effects.
    ._eval_multi = function() {

      private$._scope$k <- private$._loop_struct$matrix_idx

      if (!private$._loop_struct$looping) {

        private$._eval_fns()
        return(
          private$._format_list_of_evals(private$._fns_outcome_formatted)
        )
      }


      if (private$._loop_struct$col_looping_first) {

        private$._eval_by_col_groups(inner = FALSE)
        return(private$._col_outcome)
      }

      private$._eval_by_row_groups(inner = FALSE)

      if (private$._margin == 0 && private$._loop_struct$row_grouped &&
          private$._loop_struct$col_grouped && private$._simplify == "no") {

        return(tidyr::unnest(private$._row_outcome, cols = .vals))
      }

      private$._row_outcome

    },




    #' Private method
    #'
    #' Execute the function evaluation process by looping across row groups
    #' (which would mean looping across all rows for the case of apply_row and
    #' dfw/dfl variants) and returns the formatted outcome.
    #'
    #' This function is a subcontractor and is called by either
    #' 1. ._eval_by_matrix() or ._eval_multi(), with the former handling the
    #'    loops upon matrices, when row looping is the outer loop (or second
    #'    loop when there is also matrix looping.
    #' 2. ._eval_by_col_groups(), when the row looping is the inner loop.
    #'
    #' Based on the looping template structure, it will also perform the loops
    #' across columns , by subcontracting the job to ._eval_by_col_groups(),
    #' when both loops are necessary and the row loop is the outer loop.
    #'
    #' @param inner    bool; is the row loop the outer loop (FALSE) or inner
    #'                 loop (TRUE)?
    #'
    #' @returns
    #' nothing; used for its side effects.
    ._eval_by_row_groups = function(inner = FALSE) {

      private$._reset_row_outcome()

      for (ridx in seq_along(private$._loop_struct$row_groups_for_loop)) {

        if (private$._loop_struct$row_grouped)
          private$._reset_fns_outcome_names()

        private$._scope$i <- private$._loop_struct$row_groups_for_loop[[ridx]]

        if (inner || is.null(private$._loop_struct$col_groups_for_loop)) {
          private$._eval_fns(private$._loop_struct$row_grouped)
          private$._row_outcome[[ridx]] <- private$._fns_outcome_formatted
          next
        }

        private$._eval_by_col_groups(!inner)
        private$._row_outcome[[ridx]] <- private$._col_outcome

      }

      private$._format_margin_outcome("row")

    },



    #' Private method
    #'
    #' Execute the function evaluation process by looping across column groups
    #' (which would mean looping across all columns for the case of
    #' apply_column and dfw/dfl variants) and returns the formatted outcome.
    #'
    #' This function is a subcontractor and is called by either
    #' 1. ._eval_by_matrix() or ._eval_multi(), with the former handling the
    #'    loops upon matrices, when column looping is the outer loop (or second
    #'    loop when there is also matrix looping.
    #' 2. ._eval_by_row_groups(), when the column looping is the inner loop.
    #'
    #' Based on the looping template structure, it will also perform the loops
    #' across rows , by subcontracting the job to ._eval_by_row_groups(),
    #' when both loops are necessary and the column loop is the outer loop.
    #'
    #' @param inner    bool; is the column loop the outer loop (FAlSE) or the
    #'                 inner loop (TRUE)?
    #'
    #' @returns
    #' nothing; used for its side effects.
    ._eval_by_col_groups = function(inner = TRUE) {

      private$._reset_col_outcome()

      for (cidx in seq_along(private$._loop_struct$col_groups_for_loop)) {

        if (private$._loop_struct$col_grouped)
          private$._reset_fns_outcome_names()

        private$._scope$j <- private$._loop_struct$col_groups_for_loop[[cidx]]

        if (inner || is.null(private$._loop_struct$row_groups_for_loop)) {
          private$._eval_fns(private$._loop_struct$col_grouped)
          private$._col_outcome[[cidx]] <- private$._fns_outcome_formatted
          next
        }

        private$._eval_by_row_groups(!inner)
        private$._col_outcome[[cidx]] <- private$._row_outcome
      }


      private$._format_margin_outcome("col")

    },




    #' Private method
    #'
    #' Formats the outcome vector of a margin.
    #'
    #' It is essentially a wrapper to ._format_list_of_evals(), where it is
    #' applied per group when it applies. In the latter case, row binding is
    #' applied without column id, as it is not necessary in the context of
    #' grouping, given that the groups are already identified by a set of
    #' columns.
    #'
    #' In the case of simplification in grouping context, an unnest step is
    #' perform because otherwise, the output tibble will contains tibbles and
    #' the simplification steps that comes later won't work.
    #'
    #' @param margin    one of "row" or "col", margin to format
    #'
    #' @returns
    #' nothing; used for its side effects.
    ._format_margin_outcome = function(margin) {

      grouped <- private$._loop_struct[[paste(margin, "grouped", sep = "_")]]
      outcome_id <- paste(".", margin, "outcome", sep = "_")

      if (grouped) {
        outcome_tmp <- private$._loop_struct[[paste(margin, "group_df", sep = "_")]]

        outcome_tmp$.rows <- lapply(private[[outcome_id]], function(o) {
          private$._format_list_of_evals(o, no_id = TRUE)
        })

        private[[outcome_id]] <- dplyr::rename(outcome_tmp, .vals = .rows)
        if (private$._simplify != "no") {
          private[[outcome_id]] <- tidyr::unnest(private[[outcome_id]],
                                                 cols = .vals)
        }
        return()

      }

      private[[outcome_id]] <- private$._format_list_of_evals(private[[outcome_id]])

    },




    #' Private method
    #'
    #' Formats the outcome vector of function evaluation.
    #'
    #' It does one of two things:
    #'
    #' 1. returns the vector unchanged or
    #' 2. returns a tibble that is the result of row binding the elements. This
    #'    is done if simplification is requested by the user.
    #'
    #'  The default in row binding is to provide an identifyer column, but this
    #'  can be disabled with `no_id = TRUE`. This is used for instance in the
    #'  context of grouping, where the identifyer column is not needed.
    #'
    #' @param lst    a list, although any vector can technically do. This is the
    #'               outcome vector of the evaluated functions.
    #' @param no_id  bool, if TRUE, no column identifyer is created when row
    #'               binding. Applies only in the context of simplification.
    #'
    #' @returns
    #' a list or a tibble; see function description.
    ._format_list_of_evals = function(lst, no_id = FALSE) {

      if (private$._simplify == "no") return(lst)
      if (no_id) return(dplyr::bind_rows(lst))
      dplyr::bind_rows(lst, .id = private$._tag)

    },




    #' Private method
    #'
    #' Evaluates the functions and performs initial formatting.
    #'
    #' The formatting at this stage is minimal and is simply staging the real
    #' formatting that will come later.
    #'
    #' 1. In the context of simplification in long format, removes the vector
    #'    names, as they are not needed given the ID column that is created in
    #'    that context.
    #'    ._format_fn_outcome() performs that action
    #' 2. Once all the functions have been evaluated, if simplification is
    #'    needed, assesses that all lengths are the same (via ._assess_length()),
    #'    a necessary condition to perform the following formatting
    #' 3. In the context of simplification, reformat as lists where elements
    #'    will be tibble columns once gone through the bind_rows step. This is
    #'    done via ._format_list_of_fns(); see that function for details.
    #'
    #' @param grouped    bool, TRUE if one of row or column grouping is on.
    #'                   Needed for final formatting.
    #'
    #' @returns
    #' nothing; used for its side effects.
    ._eval_fns = function(grouped = FALSE) {

      for (fidx in seq_len(private$._fns_n)) {

        fn <- private$._fns[[fidx]]
        private$._scope$register_function(fn)
        private$._fns_outcome[[fidx]] <- private$._scope$eval()
        private$._set_out_lens(fidx)
        private$._format_fn_outcome(fidx)

      }

      private$._assess_length()
      private$._format_list_of_fns(grouped)

    },




    #' Private method
    #'
    #' Updates the idx^th function outcome length
    #'
    #' @param idx   index (integer), which function to update
    #'
    #' @returns
    #' nothing; used for its side effects.
    ._set_out_lens = function(idx) {

      private$._fn_out_lens[idx] <- length(private$._fns_outcome[[idx]])

    },



    #' Private method
    #'
    #' Updates the idx^th function outcome names.
    #'
    #' These are the names that will make the function IDs unique within the
    #' outcome of the same function (e.g., range returns of vector of length 2,
    #' the naming will be unique after ._set_fns_names).
    #'
    #' Done only if:
    #'
    #' 1. Simplification
    #' 2. function returns a vector of length > 1 - or if force_name is TRUE
    #' 3. the names have not been set already. This is for optimization purpose,
    #'    as this step is peformed within loops
    #'
    #' @param idx   index (integer), which function to update
    #'
    #' @returns
    #' nothing; used for its side effects.
    ._set_fns_names = function(idx) {

      if (private$._simplify != "no" &&
          (private$._fn_out_lens[idx] != 1L || private$._force_name) &&
          is.null(out_names <- private$._fns_outcome_names[[idx]])) {

        out_names <- make_names(private$._fns_outcome[[idx]], .name = "")
        private$._fns_outcome_names[[idx]] <- out_names
      }

    },




    #' Private method
    #'
    #' In the context of simplification in long format, removes the outcome
    #' vector names of the idx^th element, as they are not needed given the ID
    #' column that is created in that context.
    #'
    #' @param idx    index of the outcome element to update.
    #'
    #' @returns
    #' nothing; used for its side effects.
    ._format_fn_outcome = function(idx) {

      private$._set_fns_names(idx)

      if (private$._simplify == "long") {
        private$._fns_outcome[[idx]] <- unname(private$._fns_outcome[[idx]])
        return()
      }

      invisible()

    },



    #' Private method
    #'
    #' If simplification is needed, assesses that all lengths of the outcome
    #' vector are the same (via ._assess_length()), a necessary condition in
    #' order to perform simplification.
    #'
    #' @returns
    #' nothing; used for its side effects.
    ._assess_length = function() {

      if (private$._simplify == "no") return(invisible())


      lens <- private$._fn_out_lens

      any0 <- any(l0 <- lens == 0L)
      if (any0) lens <- lens[-l0]
      multi <- if (length(lens) > 0) {
        length(unique(lens)) > 1
      } else FALSE

      if (multi) stop("vectors must be of the same length", call. = FALSE)

      invisible()

    },




    #' Private method
    #'
    #' In the context of simplification, performs formatting so that the
    #' bind_rows step will work and yield the long/wide format that is needed.
    #'
    #' Otherwise, or when formatting is irrelevant (i.e., when the outcomes
    #' are all of length 1 without name forcing), ._fns_outcome_formatted is
    #' simply equal to ._fns_outcome.
    #'
    #' See the comments in the function for the long and wide specificity of
    #' the formats.
    #'
    #' Finally, when grouped, the outcome is wrapped in a list as a measure of
    #' protection against bind_rows.
    #'
    #' @param idx    bool; was there row or column grouping?
    #'
    #' @returns
    #' nothing; used for its side effects.
    ._format_list_of_fns = function(grouped) {


      if (private$._simplify == "no") {
        private$._fns_outcome_formatted <- private$._fns_outcome
        return()
      }

      # with length == 1 and no name forcing, it long/wide format is irrelevant
      if (unique(private$._fn_out_lens) == 1L && !private$._force_name) {
        if (private$._margin == 0) {
          private$._fns_outcome_formatted <- list(private$._fns_outcome)
          return()
        }
        private$._fns_outcome_formatted <- private$._fns_outcome
        return()
      }


      # creates new elements - that will later become new columns - that
      # contains the IDs of the outcomes. It is also ordered to make sure the
      # "name" element always comes right before its corresponding outcome, as
      # well as keeping the original outcome order.
      if (private$._simplify == "long") {
        nms <- setNames(private$._fns_outcome_names,
                        paste0(names(private$._fns_outcome), ".name"))
        private$._fns_outcome_formatted <- c(nms, private$._fns_outcome)[private$._long_order]

        if (grouped || private$._margin == 0) {
          private$._fns_outcome_formatted <- list(private$._fns_outcome_formatted)
        }
        return()
      }

      # wide

      # The function outcomes are in a list where each element corresponds to
      # a function. The flattening step creates a single vector (which could be
      # a list) where each element will be converted to a wide-wise column
      # later. In addition, based on ._fns_outcome_names, the new "columns" are
      # renamed so that their names are unique.
      #
      # As for list_row, it wraps lists in an additional layer of list but
      # doesn't do anything to vectors. This is to protect lists against the
      # unnesting step that comes later.
      private$._fns_outcome_formatted <- list_row(
        flatten_and_name(private$._fns_outcome,
                         private$._fns_outcome_names))

      if (grouped || private$._margin == 0) {
        private$._fns_outcome_formatted <- list(private$._fns_outcome_formatted)
      }

    }

  )
)




#' FnMaker: Harmonizes provided functions into function expressions.
#'
#' Turns functions and formulas into function expressions, as well as making
#' sure that provided function names do not clash with matrixset row/column
#' tags - though the later is relevant only in context of simplification.
#'
#' @docType class
#' @noRd
#' @name FnMaker
FnMaker <- R6::R6Class(
  "FnMaker",

  public = list(


    #' Constructor for FnMaker
    #'
    #' @param .ms       The matrix_set object in which to get row and/or column
    #'                  tag and thus, to which the FnMaker class is assigned to
    #' @param quos      the functions provided by the user, given to FnMaker as
    #'                  quosures.
    #' @param margin    0, 1 or 2. 0 = `apply_matrix` has been called.
    #'                  1 = `apply_row` and 2 = `apply_column`. The dfw/dfl
    #'                  versions are covered as well. For FnMaker, the margin
    #'                  is needed to know which tag (row or column) to use to
    #'                  check that no function names are clashing with the tags.
    #' @param simplify  single character, one of "no", "long" or "wide".
    #'                  For FnMaker, simply needed to know if simplification is
    #'                  required to know if it is required to prevent function
    #'                  names clashing with row and/or column tag (typically,
    #'                  .rowname or .colname). Without simplification, there is
    #'                  no such constraints.
    #' @param call_env  The calling environment of the apply_* functions. This
    #'                  will only be used in an error message if any functions,
    #'                  provided as formulas, can't be converted to actual
    #'                  functions.
    #'
    initialize = function(.ms, quos, margin, simplify,
                          call_env = rlang::caller_env()) {

      private$._fn_names <- private$._get_fn_names(quos)
      private$._assess_fun_names(.ms, margin, simplify)

      var_tag <- get_var_tag(margin)
      private$fns_ <- lapply(quos, function(q) {
        private$._as_fn_expr(q, var_tag, call_env)
      })

      names(private$fns_) <- private$._fn_names

    }

  ),

  active = list(

    #' Functions
    #'
    #' @field fns    expressions representing the functions to evaluate, in a
    #'               list format. Each list name is the function name, or label.
    fns = function() private$fns_

  ),



  private = list(

    fns_ = NULL,          # expressions representing the functions to evaluate,
                          # in a list format.

    ._fn_names = NULL,    # name, or label, of each function expression.



    #' Private method
    #'
    #' Test if a quosure is a formula.
    #'
    #' @param quo    quosure to test
    #'
    #' @returns
    #' bool, TRUE if the quosure is a formula, FALSE otherwise
    ._quo_is_formula = function(quo) {
      rlang::is_formula(rlang::quo_get_expr(quo))
    },



    #' Private method
    #'
    #' Test if a quosure is a function.
    #'
    #' @param quo    quosure to test
    #'
    #' @returns
    #' bool, TRUE if the quosure is a function, FALSE otherwise
    ._quo_is_function = function(quo) {
      if (!rlang::quo_is_symbolic(quo)) return(FALSE)
      test_for_fn <- tryCatch(rlang::eval_tidy(quo), error = function(e) e)
      if (inherits(test_for_fn, "error")) return(FALSE)
      rlang::is_function(test_for_fn)
    },



    #' Private method
    #'
    #' Get names (labels) of provided functions. For formulas, the ~ character
    #' is removed.
    #'
    #' @param quos    the quosures with the function expressions
    #'
    #' @returns
    #' character vector
    ._get_fn_names = function(quos) {
      nmfn <- names(quos)
      is_a_formula <- vapply(quos, private$._quo_is_formula, FALSE)
      if (any(form_idx <- is_a_formula)) {
        nmfn[form_idx] <- gsub("^~", "", nmfn[form_idx])
      }
      nmfn
    },



    #' Private method
    #'
    #' Workhorse of ._assess_fun_names(); provides the assessment for a given
    #' margin tag.
    #'
    #' @param tag       string, the tag to check. If NULL, no check is performed
    #' @param simplify  logical (no default). If TRUE, the test is performed,
    #'                  otherwise it is skipped.
    #'
    #' @returns
    #' used for its side effect.
    ._assess_fun_names_for_tag = function(tag, simplify) {
      if (is.na(tag) || !simplify) return(invisible(NULL))
      if (any(private$._fn_names == tag))
        stop(paste("the function results can't be named", shQuote(tag)))
    },





    #' Private method
    #'
    #' Assess if the name (i.e., label) attributed to function result in apply_*
    #' (e.g. avr in apply_row_dfl(ms_object, avr = mean)) matches the matrixset
    #' object's relevant tag (tag). In the example, the relevant tag is the row
    #' tag (because apply_row), which unless changed by user, is .rowname.
    #'
    #' Note that this name is used as column name in the dfl/dfw versions of the
    #' apply functions. Consequently, the test is relevant only for these apply
    #' versions.
    #'
    #' @param .ms       The matrix_set object in which to get row and/or column
    #'                  tag.
    #' @param margin    0, 1 or 2. 0 = `apply_matrix` has been called.
    #'                  1 = `apply_row` and 2 = `apply_column`. The dfw/dfl
    #'                  versions are covered as well. For FnMaker, the margin
    #'                  is needed to know which tag (row or column) to use to
    #'                  check that no function names are clashing with the tags.
    #' @param simplify  One of "no", "long" or "wide". Will be turned into a
    #'                  bool by simplify != "no".
    #'
    #' @returns
    #' used for its side effect.
    ._assess_fun_names = function(.ms, margin, simplify) {

      # make sure we're not giving it the same name as tag. The check is skipped
      # if simplify is FALSE (not dfl/dfw), as this restriction is necessary
      # only to make sure the result tibble in dfl/dfw have column name conflict.
      if (!is.null(margin) && (margin == 0 || margin == 1)) {
        private$._assess_fun_names_for_tag(.rowtag(.ms), simplify != "no")
      }
      if (!is.null(margin) && (margin == 0 || margin == 2)) {
        private$._assess_fun_names_for_tag(.coltag(.ms), simplify != "no")
      }

    },



    #' Private method
    #'
    #' Turn a quosure representing a function into an expression representing a
    #' function.
    #'
    #' The quosure can be either a formula or a function.
    #'
    #' @param x          quosure to convert
    #' @param dot_arg    the pronoun that is available to use (typically, .m, .i
    #'                   or .j) by the functions. Only used if need to convert
    #'                   the function into an expression.
    #'                   E.g., mean will become mean(.i) for apply_row.
    #' @param call       The calling environment of the apply_* functions. This
    #'                   will only be used in an error message if any functions,
    #'                   provided as formulas, can't be converted to actual
    #'                   functions.
    #' @param arg        This argument will be mentioned in error messages as
    #'                   the input that is at the origin of a problem.
    #'
    #' @returns
    #' bool, TRUE if the quosure is a formula, FALSE otherwise
    ._as_fn_expr = function(x, dot_arg, call, arg = rlang::caller_arg(x)) {

      if (private$._quo_is_function(x)) {
        return(rlang::call2(rlang::quo_get_expr(x), as.name(dot_arg)))

      }

      if (private$._quo_is_formula(x)) {

        fn <- rlang::as_function(rlang::eval_tidy(x), env = globalenv(),
                                 arg = arg, call = call)
        return(body(fn))

      }

      lifecycle::deprecate_warn("0.4.0",
                                I("Providing expressions"),
                                with = I("a formula"),
                                always = TRUE)
      return(rlang::quo_get_expr(x))


      # will be active once the lifecycle permanently disallows expressions
      rlang::abort("Functions must be provided as formulas or function names.")

    }

  ))






# margin = NULL: mult
#          0: whole matrix
#          1: rowwise
#          2: colwise
eval_function <- function(.ms, ..., margin = NULL, matidx = NULL,
                          .matrix_wise = TRUE, .input_list = FALSE,
                          .simplify = "no", .force_name = FALSE,
                          env = rlang::caller_env(2))
{
  lifecycle::deprecate_soft("0.4.0", I("Formatting NULL matrices"), user_env = env)

  quosures <- rlang::enquos(..., .named = TRUE, .ignore_empty = "all")
  fn_maker <- FnMaker$new(.ms, quosures, margin, .simplify)


  applyer <- Applyer$new(.ms, matidx, margin, fn_maker$fns, .matrix_wise,
                         .input_list, .simplify, .force_name, env)

  applyer$eval()

}





#' Apply functions to each matrix of a matrixset
#'
#' @description
#' The `apply_matrix` function applies functions to each matrix of a `matrixset`.
#' The `apply_row`/`apply_column` functions do the same but separately for each
#' row/column. The functions can be applied to all matrices or only a subset.
#'
#' The `dfl`/`dfw` versions differ in their output format and when possible,
#' always return a [tibble::tibble()].
#'
#' Empty matrices are simply left unevaluated. How that impacts the returned
#' result depends on which flavor of apply_* has been used. See \sQuote{Value}
#' for more details.
#'
#' If `.matrix_wise` is `FALSE`, the function (or expression) is multivariate in
#' the sense that all matrices are accessible at once, as opposed to each of them
#' in turn.
#'
#' See section "Multivariate".
#'
#' @section Pronouns:
#' The `rlang` pronouns `.data` and `.env` are available. Two scenarios for
#' which they can be useful are:
#'  * The annotation names are stored in a character variable. You can make use
#'      of the variable by using `.data[[var]]`. See the example for an
#'      illustration of this.
#'  * You want to make use of a global variable that has the same name as an
#'      annotation. You can use `.env[[var]]` or `.env$var` to make sure to use
#'      the proper variable.
#'
#' The matrixset package defines its own pronouns: `r var_lab_mat`,
#' `r var_lab_row` and `r var_lab_col`, which
#' are discussed in the function specification argument (`...`).
#'
#' It is not necessary to import any of the pronouns (or load `rlang` in the
#' case of `.data` and `.env`) in a interactive session.
#'
#' It is useful however when writing a package to avoid the `R CMD check` notes.
#' As needed, you can import `.data` and `.env` (from `rlang`) or any of `r var_lab_mat`,
#' `r var_lab_row` or `r var_lab_col` from `matrixset`.
#'
#' @section Multivariate:
#' The default behavior is to apply a function or expression to a single
#' matrix and each matrices of the `matrixset` object are provided sequentially
#' to the function/expression.
#'
#' If `.matrix_wise` is `FALSE`, all matrices are provided at once to the
#' functions/expressions. They can be provided in two fashions:
#'  * separately (default behavior). Each matrix can be referred by `.m1`, ...,
#'      `.mn`, where `n` is the number of matrices. Note that this is the number
#'       as determined by `.matrix`.
#'
#'       For `apply_row` (and dfl/dfw variants), use `.i1`, `.i2` and so on
#'       instead. What the functions/expressions have access to in this case is
#'       the first row of the first matrix, the first row of the second matrix
#'       and so on. Then, continuing the loop, the second row of each matrix
#'       will be accessible, and so on
#'
#'       Similarly, use `.j1` and so on for the `apply_column` family.
#'
#'       Anonymous functions will be understood as a function with multiple
#'       arguments. In the example `apply_row(ms, mean, .matrix_wise = FALSE)`,
#'       if there are 3 matrices in the `ms` object, `mean` is understood as
#'       `mean(.i1, .i2, .i3)`. Note that this would fail because of the `mean`
#'       function.
#'
#'  * In a list (`.list_input = TRUE`). The list will have an element per matrix.
#'     The list can be referred using the same pronouns (`.m`, `.i`, `.j`), and
#'     the matrix, by the matrix names or position.
#'
#' For the multivariate setting, empty matrices are given as is, so it is
#' important that provided functions can deal with such a scenario. An
#' alternative is to skip the empty matrices with the `.matrix` argument.
#'
#' @section Grouped matrixsets:
#' If groups have been defined, functions will be evaluated within them. When
#' both row and column grouping has been registered, functions are evaluated at
#' each cross-combination of row/column groups.
#'
#' The output format is different when the `.ms` matrixset object is grouped.
#' A list for every matrix is still returned, but each of these lists now holds
#' a tibble.
#'
#' Each tibble has a column called `.vals`, where the function results are
#' stored. This column is a list, one element per group. The group labels are
#' given by the other columns of the tibble. For a given group, things are like
#' the ungrouped version: further sub-lists for rows/columns - if applicable -
#' and function values.
#'
#' The dfl/dfw versions are more similar in their output format to their
#' ungrouped version. The format is almost identical, except that additional
#' columns are reported to identify the group labels.
#'
#' See the examples.
#'
#'
#' @param .ms    `matrixset` object
#' @param ...    expressions, separated by commas. They can be specified in one of
#'     the following way:
#'
#'    * a function name, e.g., `mean`.
#'    * An anonymous function, e.g., `function(x) mean(x)` or `\(x) mean(x)`
#'    * a formula expression, which may represent a function call, where you can
#'       use `r var_lab_mat` to represent the current matrix (for `apply_matrix`),
#'       `r var_lab_row` to represent the current row (for `apply_row`) and
#'       `r var_lab_col` for the current column (`apply_column`). Bare names of
#'       object traits can be used as well. For instance, `~ lm(.i ~ program)`.
#'
#'       The pronouns are also available for the multivariate version, under
#'       certain circumstances, but they have a different meaning. See the
#'       "Multivariate" section for more details.
#'    * `r lifecycle::badge("superseded")`  an expression. Superseded in favor
#'       of using a formula. The usage is almost identical and the formula is
#'       more clear.
#'
#'    The expressions can be named; these names will be used to provide names to
#'    the results.
#'
#' @param .matrix   matrix indices of which matrix to apply functions to. The
#'                  default, `NULL`, means all the matrices are used.
#'
#'    If not `NULL`, index is numeric or character vectors.
#'
#'    Numeric values are coerced to integer as by [as.integer()] (and hence
#'    truncated towards zero).
#'
#'    Character vectors will be matched to the matrix names of the object.
#'
#'    Can also be logical vectors, indicating elements/slices to replace. Such
#'    vectors are *NOT* recycled, which is an important difference with usual
#'    matrix replacement. It means that the `logical` vector must match the
#'    number of matrices in length.
#'
#'    Can also be negative integers, indicating elements/slices to leave out of
#'    the replacement.
#'
#' @param .matrix_wise    `logical`. By default (`TRUE`), matrices are provided
#'    one by one, in turn, to the functions/expressions. But if `.matrix_wise` is
#'    `FALSE`, the functions/expressions have access to all matrices. See
#'    "Multivariate" for details, including how to reference the matrices.
#'
#' @param .input_list    `logical`. If multivariate (`.matrix_wise ==  FALSE`),
#'    the matrices are provided as a single list, where each element is a matrix
#'    (or matrix row or column). The list elements are the matrix names.
#'
#' @param .force_name    `logical`. Used only for the simplified output versions
#'    (dfl/dfw). By default (`FALSE`), function IDs will be provided only if the
#'    function outcome is a vector of length 2 or more. If `.force_name` is
#'    `TRUE` then function IDs are provided in all situations.
#'
#'    This can be useful in situation of grouping. As the functions are
#'    evaluated independently within each group, there could be situations where
#'    function outcomes are of length 1 for some groups and lenght 2 or more in
#'    other groups.
#'
#'    See examples.
#'
#' @returns
#' A list for every matrix in the matrixset object. Each list is itself a
#' list, or `NULL` for `NULL` matrices. For `apply_matrix`, it is a list of
#' the function values. Otherwise, it is a list with one element for each
#' row/column. And finally, for `apply_row`/`apply_column`, each of these
#' sub-list is a list, the results of each function.
#'
#' When `.matrix_wise == FALSE`, the output format differs only in that there is
#' no list for matrices.
#'
#' If each function returns a `vector` of the same dimension, you can use either
#' the `_dfl` or the `_dfw` version. What they do is to return a list of
#' `tibble`s. The `dfl` version will stack the function results in a long format
#' while the `dfw` version will put them side-by-side, in a wide format. An
#' empty matrix will be returned for empty input matrices.
#'
#' If the functions returned vectors of more than one element, there will be a
#' column to store the values and one for the function ID (dfl), or one column
#' per combination of function/result (dfw)
#'
#' See the grouping section to learn about the result format in the grouping
#' context.
#'
#' @examples
#' # The firs example takes the whole matrix average, while the second takes
#' # every row average
#' (mn_mat <- apply_matrix(student_results, mean))
#' (mn_row <- apply_row(student_results, mean))
#'
#' # More than one function can be provided. It's a good idea in this case to
#' # name them
#' (mn_col <- apply_column(student_results, avr=mean, med=median))
#'
#' # the dfl/dfw versions returns nice tibbles - if the functions return values
#' # of the same length.
#' (mn_l <- apply_column_dfl(student_results, avr=mean, med=median))
#' (mn_w <- apply_column_dfw(student_results, avr=mean, med=median))
#'
#' # There is no difference between the two versions for length-1 vector results.
#' # hese will differ, however
#' (rg_l <- apply_column_dfl(student_results, rg=range))
#' (rg_w <- apply_column_dfw(student_results, rg=range))
#'
#' # More complex examples can be used, by using pronouns and data annotation
#' (vals <- apply_column(student_results, avr=mean, avr_trim=~mean(.j, trim=.05),
#'                                       reg=~lm(.j ~ teacher)))
#'
#' # You can wrap complex function results, such as for lm, into a list, to use
#' # the dfl/dfr version
#' (vals_tidy <- apply_column_dfw(student_results, avr=mean, avr_trim=~mean(.j, trim=.05),
#'                                                reg=~list(lm(.j ~ teacher))))
#'
#' # You can provide complex expressions by using formulas
#' (r <- apply_column(student_results,
#'                                   res= ~ {
#'                                     log_score <- log(.j)
#'                                     p <- predict(lm(log_score ~ teacher + class))
#'                                     .j - exp(p)
#'                                   }))
#'
#' # the .data pronoun can be useful to use names stored in variables
#' fn <- function(nm) {
#'   if (!is.character(nm) && length(nm) != 1) stop("this example won't work")
#'   apply_column(student_results, ~lm(.j ~ .data[[nm]]))
#' }
#' fn("teacher")
#'
#' # You can use variables that are outside the scope of the matrixset object.
#' # You don't need to do anything special if that variable is not named as an
#' # annotation
#' pass_grade <- 0.5
#' (passed <- apply_row_dfw(student_results, pass = ~ .i >= pass_grade))
#'
#' # use .env if shares an annotation name
#' previous_year_score <- 0.5
#' (passed <- apply_row_dfw(student_results, pass = ~ .i >= .env$previous_year_score))
#'
#' # Grouping structure makes looping easy. Look at the output format
#' cl_prof_gr <- row_group_by(student_results, class, teacher)
#' (gr_summ <- apply_column(cl_prof_gr, avr=mean, med=median))
#' (gr_summ_tidy <- apply_column_dfw(cl_prof_gr, avr=mean, med=median))
#' # to showcase how we can play with format
#' (gr_summ_tidy_long <- apply_column_dfl(cl_prof_gr, summ = ~ c(avr=mean(.j), med=median(.j))))
#'
#' # It is even possible to combine groupings
#' cl_prof_program_gr <- column_group_by(cl_prof_gr, program)
#' (mat_summ <- apply_matrix(cl_prof_program_gr, avr = mean, med = median, rg = range))
#' # it doesn' make much sense, but this is to showcase format
#' (summ_gr <- apply_matrix(cl_prof_program_gr, avr = mean, med = median, rg = range))
#' (summ_gr_long <- apply_column_dfl(cl_prof_program_gr,
#'                                  ct = ~ c(avr = mean(.j), med = median(.j)),
#'                                  rg = range))
#' (summ_gr_wide <- apply_column_dfw(cl_prof_program_gr,
#'                                  ct = ~ c(avr = mean(.j), med = median(.j)),
#'                                  rg = range))
#'
#'
#' # This is an example where you may want to use the .force_name argument
#' (apply_matrix_dfl(column_group_by(student_results, program), FC = ~ colMeans(.m)))
#' (apply_matrix_dfl(column_group_by(student_results, program), FC = ~ colMeans(.m),
#'                   .force_name = TRUE))
#'
#' @name loop
NULL






#' @rdname loop
#' @export
apply_row <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                      .input_list = FALSE)
  UseMethod("apply_row")

#' @export
apply_row.matrixset <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                                .input_list = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin = 1, matidx = .matrix,
                .matrix_wise = .matrix_wise, .input_list = .input_list)
}



#' @rdname loop
#' @export
apply_row_dfl <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                          .input_list = FALSE, .force_name = FALSE)
  UseMethod("apply_row_dfl")

#' @export
apply_row_dfl.matrixset <- function(.ms, ..., .matrix = NULL,
                                    .matrix_wise = TRUE, .input_list = FALSE,
                                    .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin=1, matidx = .matrix, .simplify = "long",
                .force_name = .force_name, .matrix_wise = .matrix_wise,
                .input_list = .input_list)
}



#' @rdname loop
#' @export
apply_row_dfw <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                          .input_list = FALSE, .force_name = FALSE)
  UseMethod("apply_row_dfw")

#' @export
apply_row_dfw.matrixset <- function(.ms, ..., .matrix = NULL,
                                    .matrix_wise = TRUE, .input_list = FALSE,
                                    .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin = 1, matidx = .matrix, .simplify = "wide",
                .force_name = .force_name, .matrix_wise = .matrix_wise,
                .input_list = .input_list)
}





#' @rdname loop
#' @export
apply_column <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                         .input_list = FALSE)
  UseMethod("apply_column")

#' @export
apply_column.matrixset <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                                   .input_list = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin=2, matidx = .matrix,
                .matrix_wise = .matrix_wise, .input_list = .input_list)
}



#' @rdname loop
#' @export
apply_column_dfl <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                             .input_list = FALSE, .force_name = FALSE)
  UseMethod("apply_column_dfl")

#' @export
apply_column_dfl.matrixset <- function(.ms, ..., .matrix = NULL,
                                       .matrix_wise = TRUE, .input_list = FALSE,
                                       .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin=2, matidx = .matrix, .simplify = "long",
                .force_name = .force_name, .matrix_wise = .matrix_wise,
                .input_list = .input_list)
}




#' @rdname loop
#' @export
apply_column_dfw <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                             .input_list = FALSE, .force_name = FALSE)
  UseMethod("apply_column_dfw")

#' @export
apply_column_dfw.matrixset <- function(.ms, ..., .matrix = NULL,
                                       .matrix_wise = TRUE, .input_list = FALSE,
                                       .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin=2, matidx = .matrix, .simplify = "wide",
                .force_name = .force_name, .matrix_wise = .matrix_wise,
                .input_list = .input_list)
}




#' @rdname loop
#' @export
apply_matrix <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                         .input_list = FALSE)
  UseMethod("apply_matrix")


#' @export
apply_matrix.matrixset <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                                   .input_list = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin = 0, matidx = .matrix,
                .matrix_wise = .matrix_wise, .input_list = .input_list)
}





#' @rdname loop
#' @export
apply_matrix_dfl <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                             .input_list = FALSE, .force_name = FALSE)
  UseMethod("apply_matrix_dfl")


#' @export
apply_matrix_dfl.matrixset <- function(.ms, ..., .matrix = NULL,
                                       .matrix_wise = TRUE, .input_list = FALSE,
                                       .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin = 0, matidx = .matrix, .simplify = "long",
                .force_name = .force_name, .matrix_wise = .matrix_wise,
                .input_list = .input_list)
}






#' @rdname loop
#' @export
apply_matrix_dfw <- function(.ms, ..., .matrix = NULL, .matrix_wise = TRUE,
                             .input_list = FALSE, .force_name = FALSE)
  UseMethod("apply_matrix_dfw")


#' @export
apply_matrix_dfw.matrixset <- function(.ms, ..., .matrix = NULL,
                                       .matrix_wise = TRUE, .input_list = FALSE,
                                       .force_name = FALSE)
{
  warn_if(.matrix_wise, .input_list)
  eval_function(.ms, ..., margin = 0, matidx = .matrix, .simplify = "wide",
                .force_name = .force_name, .matrix_wise = .matrix_wise,
                .input_list = .input_list)
}



