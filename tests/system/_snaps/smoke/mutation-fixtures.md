# mutation results for R.methodsS3 remain stable

    {
      "outcome": "OK",
      "summary": "generated=1379 | tested=10 | killed=2 | hanged=0 | survived=8 | mutation_score=20 | mutation_score_ci=5.66821514543752,50.9837528463358 | confidence=0.95",
      "mutants": "030.setMethodS3.R_030.setMethodS3.R_052.R | R/030.setMethodS3.R | SURVIVED | 110 | 5 | 110 | 54 | '[[' -> 'NULL' | const_null\nmakeNamespace.R_makeNamespace.R_058.R | R/makeNamespace.R | SURVIVED | 20 | 11 | 20 | 58 | 'S3method(\"%s\", \"%s\")\n' -> 'NA_character_' | na_replace\nrccValidators.R_rccValidators.R_047.R | R/rccValidators.R | SURVIVED | 16 | 7 | 16 | 81 | 'stop(\"Method/function names should start with a lower case letter: \", ' -> 'invisible(NULL)' | error_handling\n030.setMethodS3.R_030.setMethodS3.R_232.R | R/030.setMethodS3.R | KILLED | 199 | 3 | 199 | 67 | 'envir' -> '<default>' | named_arg_drop\nrccValidators.R_rccValidators.R_006.R | R/rccValidators.R | KILLED | 6 | 28 | 6 | 50 | '' -> 'NULL' | const_null\n030.setMethodS3.R_030.setMethodS3.R_193.R | R/030.setMethodS3.R | SURVIVED | 177 | 3 | 182 | 3 | 'for' -> '<deleted>' | stmt_delete\nzzz.R_zzz.R_013.R | R/zzz.R | SURVIVED | 4 | 3 | 8 | 3 | 'options' -> '<deleted>' | stmt_delete\npkgStartupMessage.R_pkgStartupMessage.R_043.R | R/pkgStartupMessage.R | SURVIVED | 64 | 9 | 67 | 9 | 'break' -> '<deleted>' | stmt_delete\n030.setMethodS3.R_030.setMethodS3.R_025.R | R/030.setMethodS3.R | SURVIVED | 80 | 3 | 82 | 3 | 'stop' -> '<deleted>' | stmt_delete\n000.R_000.R_062.R | R/000.R | SURVIVED | 63 | 9 | 63 | 26 | '<unknown>' -> 'NA_character_' | na_replace"
    }

# mutation results for forcats remain stable

    {
      "outcome": "OK",
      "summary": "generated=860 | tested=10 | killed=8 | hanged=0 | survived=2 | mutation_score=80 | mutation_score_ci=49.0162471536642,94.3317848545625 | confidence=0.95",
      "mutants": "lvls.R_lvls.R_056.R | R/lvls.R | KILLED | 90 | 3 | 90 | 22 | 'seq_along' -> '<deleted>' | stmt_delete\nexplicit_na.R_explicit_na.R_019.R | R/explicit_na.R | KILLED | 43 | 3 | 55 | 3 | '<-' -> '<deleted>' | stmt_delete\nlump.R_lump.R_066.R | R/lump.R | KILLED | 146 | 7 | 146 | 11 | '0' -> 'NULL' | const_null\nrelevel.R_relevel.R_001.R | R/relevel.R | KILLED | 46 | 3 | 46 | 23 | '<-' -> '<deleted>' | stmt_delete\nlump.R_lump.R_025.R | R/lump.R | KILLED | 106 | 17 | 106 | 30 | '>=' -> '>' | rel_boundary\nrecode.R_recode.R_035.R | R/recode.R | KILLED | 64 | 3 | 64 | 22 | '<-' -> '<deleted>' | stmt_delete\nlump.R_lump.R_100.R | R/lump.R | KILLED | 190 | 9 | 190 | 19 | '>' -> '<' | rel_swap\nlump.R_lump.R_012.R | R/lump.R | SURVIVED | 94 | 5 | 94 | 71 | 'Must supply only one of {.arg n} and {.arg prop}.' -> 'NA_character_' | na_replace\nlump.R_lump.R_130.R | R/lump.R | SURVIVED | 221 | 3 | 226 | 3 | 'if' -> '<deleted>' | stmt_delete\nlvls.R_lvls.R_029.R | R/lvls.R | KILLED | 58 | 7 | 58 | 31 | 'anyDuplicated(new_levels)' -> '!anyDuplicated(new_levels)' | cond_negate"
    }

# mutation results for jsonlite remain stable

    {
      "outcome": "OK",
      "summary": "generated=2667 | tested=10 | killed=1 | hanged=0 | survived=9 | mutation_score=10 | mutation_score_ci=1.78762130950729,40.4150026795238 | confidence=0.95",
      "mutants": "asJSON.character.R_asJSON.character.R_010.R | R/asJSON.character.R | KILLED | 7 | 3 | 10 | 3 | 'isTRUE(keep_vec_names) && length(names(x))' -> '!(isTRUE(keep_vec_names) && length(names(x)))' | cond_negate\nwarn_keep_vec_names.R_warn_keep_vec_names.R_006.R | R/warn_keep_vec_names.R | SURVIVED | 2 | 3 | 2 | 295 | 'and named vectors will be translated into arrays instead of objects. ' -> 'NA_character_' | na_replace\ndeparse_vector.R_deparse_vector.R_063.R | R/deparse_vector.R | SURVIVED | 14 | 3 | 14 | 41 | 'TRUE' -> 'NA' | na_replace\nunescape_unicode.R_unescape_unicode.R_033.R | R/unescape_unicode.R | SURVIVED | 13 | 9 | 13 | 99 | '\\' -> 'NULL' | const_null\nflatten.R_flatten.R_010.R | R/flatten.R | SURVIVED | 33 | 7 | 33 | 21 | 'any' -> 'all' | fun_swap\nfromJSON.R_fromJSON.R_012.R | R/fromJSON.R | SURVIVED | 80 | 3 | 82 | 3 | 'stop' -> '<deleted>' | stmt_delete\nstream.R_stream.R_138.R | R/stream.R | SURVIVED | 213 | 3 | 213 | 92 | '&&' -> '||' | logic_swap\napply_by_pages.R_apply_by_pages.R_062.R | R/apply_by_pages.R | SURVIVED | 27 | 38 | 27 | 47 | 'nrow' -> 'ncol' | fun_swap\nasJSON.sf.R_asJSON.sf.R_025.R | R/asJSON.sf.R | SURVIVED | 11 | 5 | 11 | 88 | 'FALSE' -> 'NA' | na_replace\nstop.R_stop.R_001.R | R/stop.R | SURVIVED | 1 | 1 | 3 | 1 | 'FALSE' -> 'TRUE' | bool_flip"
    }

# mutation results for lumberjack remain stable

    {
      "outcome": "OK",
      "summary": "generated=739 | tested=10 | killed=7 | hanged=0 | survived=3 | mutation_score=70 | mutation_score_ci=39.6778147461145,89.2208732593699 | confidence=0.95",
      "mutants": "no_logger.R_no_logger.R_004.R | R/no_logger.R | KILLED | 44 | 9 | 44 | 34 | '<-' -> '<deleted>' | stmt_delete\nexpression_logger.R_expression_logger.R_026.R | R/expression_logger.R | KILLED | 67 | 9 | 67 | 39 | '<-' -> '<deleted>' | stmt_delete\nlumberjack.R_lumberjack.R_011.R | R/lumberjack.R | KILLED | 40 | 3 | 42 | 3 | '||' -> '&&' | logic_swap\nsimple.R_simple.R_005.R | R/simple.R | KILLED | 51 | 9 | 51 | 22 | '0' -> 'NA_real_' | na_replace\nfiledump.R_filedump.R_024.R | R/filedump.R | SURVIVED | 80 | 9 | 80 | 73 | '' -> 'NULL' | const_null\nrun.R_run.R_122.R | R/run.R | KILLED | 167 | 3 | 167 | 39 | '<-' -> '<deleted>' | stmt_delete\nlumberjack.R_lumberjack.R_045.R | R/lumberjack.R | KILLED | 62 | 3 | 62 | 30 | '!is.null(attr(data, LOGNAME))' -> 'is.null(attr(data, LOGNAME))' | not_remove\nfiledump.R_filedump.R_011.R | R/filedump.R | SURVIVED | 71 | 13 | 71 | 44 | 'TRUE' -> 'NA' | na_replace\nlumberjack.R_lumberjack.R_075.R | R/lumberjack.R | SURVIVED | 115 | 10 | 115 | 11 | '' -> 'NULL' | const_null\nlumberjack.R_lumberjack.R_125.R | R/lumberjack.R | KILLED | 279 | 8 | 279 | 19 | 'has_log(lhs)' -> '!has_log(lhs)' | cond_negate"
    }

# mutation results for nanotime remain stable

    {
      "outcome": "OK",
      "summary": "generated=3642 | tested=10 | killed=9 | hanged=0 | survived=1 | mutation_score=90 | mutation_score_ci=59.5849973204762,98.2123786904927 | confidence=0.95",
      "mutants": "nanoduration.R_nanoduration.R_361.R | R/nanoduration.R | KILLED | 329 | 15 | 329 | 84 | 'nanotime' -> 'NULL' | const_null\nnanotime.R_nanotime.R_625.R | R/nanotime.R | KILLED | 632 | 1 | 647 | 12 | '[' -> 'NULL' | const_null\nnanotime.R_nanotime.R_723.R | R/nanotime.R | KILLED | 721 | 9 | 721 | 28 | '!missing(along.with)' -> 'missing(along.with)' | not_remove\nnanotime.R_nanotime.R_020.R | R/nanotime.R | KILLED | 136 | 5 | 136 | 24 | 'new' -> '<deleted>' | stmt_delete\nnanoival.R_nanoival.R_246.R | R/nanoival.R | SURVIVED | 322 | 1 | 325 | 12 | 'nanoival' -> 'NULL' | const_null\nnanoperiod.R_nanoperiod.R_773.R | R/nanoperiod.R | KILLED | 759 | 19 | 759 | 62 | ''origin' must be of class 'nanotime'' -> 'NA_character_' | na_replace\nnanoival.R_nanoival.R_321.R | R/nanoival.R | KILLED | 381 | 1 | 384 | 12 | '+' -> 'NULL' | const_null\nnanotime.R_nanotime.R_669.R | R/nanotime.R | KILLED | 660 | 15 | 660 | 47 | 'new' -> '<deleted>' | stmt_delete\nnanoival.R_nanoival.R_351.R | R/nanoival.R | KILLED | 399 | 1 | 402 | 12 | 'nanoival' -> 'NA_character_' | na_replace\nnanoperiod.R_nanoperiod.R_605.R | R/nanoperiod.R | KILLED | 614 | 1 | 617 | 12 | 'numeric' -> 'NULL' | const_null"
    }

# mutation results for oRaklE remain stable

    {
      "outcome": "OK",
      "summary": "generated=10447 | tested=10 | killed=0 | hanged=0 | survived=10 | mutation_score=0 | mutation_score_ci=2.77555756156289e-15,27.7532799862889 | confidence=0.95",
      "mutants": "combine_models.R_combine_models.R_220.R | R/combine_models.R | SURVIVED | 109 | 7 | 109 | 111 | 'The specified data_directory does not exist: ' -> 'NULL' | const_null\nlong_term_future.R_long_term_future.R_088.R | R/long_term_future.R | SURVIVED | 43 | 11 | 43 | 44 | 'all.equal(unname(LT), expected_LT)' -> '!all.equal(unname(LT), expected_LT)' | cond_negate\ncombine_models_future.R_combine_models_future.R_232.R | R/combine_models_future.R | SURVIVED | 116 | 3 | 116 | 63 | '1' -> 'NA_real_' | na_replace\nget_historic_load_data.R_get_historic_load_data.R_250.R | R/get_historic_load_data.R | SURVIVED | 124 | 5 | 124 | 145 | 'na.rm' -> '<default>' | named_arg_drop\nlong_term_lm.R_long_term_lm.R_147.R | R/long_term_lm.R | SURVIVED | 92 | 9 | 112 | 9 | '<-' -> '<deleted>' | stmt_delete\nlong_term_future.R_long_term_future.R_611.R | R/long_term_future.R | SURVIVED | 241 | 101 | 241 | 130 | '*' -> '/' | arith_swap\nfill_missing_data.R_fill_missing_data.R_124.R | R/fill_missing_data.R | SURVIVED | 97 | 3 | 97 | 88 | 'min' -> 'max' | fun_swap\nmid_term_future.R_mid_term_future.R_019.R | R/mid_term_future.R | SURVIVED | 34 | 7 | 34 | 50 | 'example' -> 'NULL' | const_null\ndecompose_load_data.R_decompose_load_data.R_552.R | R/decompose_load_data.R | SURVIVED | 226 | 28 | 226 | 52 | '0.5' -> 'NULL' | const_null\ncombine_models.R_combine_models.R_688.R | R/combine_models.R | SURVIVED | 280 | 3 | 314 | 41 | '+' -> '-' | arith_swap"
    }

# mutation results for prettyunits remain stable

    {
      "outcome": "OK",
      "summary": "generated=1150 | tested=10 | killed=9 | hanged=0 | survived=1 | mutation_score=90 | mutation_score_ci=59.5849973204762,98.2123786904927 | confidence=0.95",
      "mutants": "rounding.R_rounding.R_027.R | R/rounding.R | KILLED | 31 | 5 | 55 | 5 | '<-' -> '<deleted>' | stmt_delete\nsizes.R_sizes.R_071.R | R/sizes.R | KILLED | 43 | 5 | 43 | 38 | '+' -> '-' | arith_swap\nsizes.R_sizes.R_032.R | R/sizes.R | KILLED | 27 | 5 | 27 | 72 | '*' -> '/' | arith_swap\npretty-package.R_pretty-package.R_002.R | R/pretty-package.R | SURVIVED | 13 | 1 | 13 | 10 | '_PACKAGE' -> 'NULL' | const_null\nnumbers.R_numbers.R_018.R | R/numbers.R | KILLED | 17 | 5 | 17 | 94 | 'u' -> 'NA_character_' | na_replace\ntime-ago.R_time-ago.R_244.R | R/time-ago.R | KILLED | 86 | 5 | 91 | 5 | '365.25' -> 'NULL' | const_null\nnumbers.R_numbers.R_223.R | R/numbers.R | KILLED | 101 | 5 | 101 | 54 | '>=' -> '>' | rel_boundary\ntime-ago.R_time-ago.R_195.R | R/time-ago.R | KILLED | 69 | 5 | 69 | 73 | 'length(date) > 1' -> '!length(date) > 1' | cond_negate\ntime-ago.R_time-ago.R_095.R | R/time-ago.R | KILLED | 37 | 3 | 48 | 3 | '<1 min' -> 'NULL' | const_null\nsizes.R_sizes.R_034.R | R/sizes.R | KILLED | 27 | 5 | 27 | 72 | '999950' -> 'NULL' | const_null"
    }

# mutation results for scales remain stable

    {
      "outcome": "OK",
      "summary": "generated=5155 | tested=10 | killed=6 | hanged=0 | survived=4 | mutation_score=60 | mutation_score_ci=31.2673769733658,83.1819670293764 | confidence=0.95",
      "mutants": "breaks-log.R_breaks-log.R_191.R | R/breaks-log.R | SURVIVED | 140 | 7 | 140 | 30 | '10' -> 'NULL' | const_null\noffset-by.R_offset-by.R_049.R | R/offset-by.R | KILLED | 45 | 3 | 45 | 37 | '<-' -> '<deleted>' | stmt_delete\ncolour-mapping.R_colour-mapping.R_302.R | R/colour-mapping.R | SURVIVED | 329 | 7 | 331 | 7 | 'Some values were outside the color scale and will be treated as NA' -> 'NULL' | const_null\npal-.R_pal-.R_040.R | R/pal-.R | SURVIVED | 186 | 20 | 186 | 60 | 'color' -> 'NA_character_' | na_replace\nlabel-number.R_label-number.R_085.R | R/label-number.R | KILLED | 305 | 19 | 305 | 34 | '0' -> 'NA_real_' | na_replace\ntransform-compose.R_transform-compose.R_027.R | R/transform-compose.R | KILLED | 29 | 9 | 29 | 30 | 'isTRUE(lower <= upper)' -> '!isTRUE(lower <= upper)' | cond_negate\nutils.R_utils.R_074.R | R/utils.R | KILLED | 73 | 3 | 73 | 21 | '1L' -> 'NA_integer_' | na_replace\nutils.R_utils.R_025.R | R/utils.R | SURVIVED | 14 | 3 | 14 | 50 | 'ggplot2' -> 'NULL' | const_null\nlabel-ordinal.R_label-ordinal.R_056.R | R/label-ordinal.R | KILLED | 112 | 23 | 112 | 61 | 'TRUE' -> 'NULL' | const_null\nlabel-number.R_label-number.R_255.R | R/label-number.R | KILLED | 441 | 32 | 446 | 3 | 'FALSE' -> 'NA' | na_replace"
    }

# mutation results for stringr remain stable

    {
      "outcome": "OK",
      "summary": "generated=1420 | tested=10 | killed=5 | hanged=0 | survived=5 | mutation_score=50 | mutation_score_ci=23.6593090512564,76.3406909487436 | confidence=0.95",
      "mutants": "interp.R_interp.R_120.R | R/interp.R | SURVIVED | 242 | 3 | 242 | 41 | '<' -> '<=' | rel_boundary\nview.R_view.R_119.R | R/view.R | KILLED | 154 | 3 | 158 | 3 | 'if' -> '<deleted>' | stmt_delete\nview.R_view.R_217.R | R/view.R | KILLED | 208 | 3 | 208 | 20 | '\n' -> 'NULL' | const_null\nmodifiers.R_modifiers.R_093.R | R/modifiers.R | KILLED | 203 | 3 | 203 | 9 | 'bound' -> 'NULL' | const_null\nview.R_view.R_176.R | R/view.R | SURVIVED | 194 | 21 | 194 | 55 | 'Empty `string` provided.\n' -> 'NA_character_' | na_replace\nmodifiers.R_modifiers.R_054.R | R/modifiers.R | KILLED | 146 | 13 | 146 | 62 | 'stringr_pattern' -> 'NULL' | const_null\nword.R_word.R_014.R | R/word.R | SURVIVED | 38 | 32 | 38 | 41 | '<' -> '<=' | rel_boundary\nview.R_view.R_163.R | R/view.R | SURVIVED | 189 | 3 | 191 | 3 | 'if' -> '<deleted>' | stmt_delete\nword.R_word.R_044.R | R/word.R | SURVIVED | 49 | 3 | 49 | 24 | '<' -> '<=' | rel_boundary\ninterp.R_interp.R_093.R | R/interp.R | KILLED | 194 | 3 | 194 | 52 | '' -> 'NA_character_' | na_replace"
    }

