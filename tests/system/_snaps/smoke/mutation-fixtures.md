# mutation results for R.methodsS3 remain stable

    {
      "outcome": "OK",
      "summary": "generated=1368 | tested=10 | killed=1 | hanged=0 | survived=9 | mutation_score=10 | mutation_score_ci=1.78762130950729,40.4150026795238 | confidence=0.95",
      "mutants": "030.setMethodS3.R_030.setMethodS3.R_053.R | R/030.setMethodS3.R | SURVIVED | 110 | 5 | 110 | 54 | '[[<-' -> 'NA_character_' | na_replace\npkgStartupMessage.R_pkgStartupMessage.R_009.R | R/pkgStartupMessage.R | SURVIVED | 39 | 5 | 39 | 20 | 'FALSE' -> 'TRUE' | bool_flip\nrccValidators.R_rccValidators.R_057.R | R/rccValidators.R | SURVIVED | 29 | 1 | 29 | 40 | 'FALSE' -> 'NA' | na_replace\n030.setMethodS3.R_030.setMethodS3.R_233.R | R/030.setMethodS3.R | SURVIVED | 199 | 3 | 199 | 67 | 'inherits' -> '<default>' | named_arg_drop\nrccValidators.R_rccValidators.R_016.R | R/rccValidators.R | SURVIVED | 8 | 3 | 8 | 62 | '$<-' -> 'NULL' | const_null\n030.setMethodS3.R_030.setMethodS3.R_194.R | R/030.setMethodS3.R | SURVIVED | 177 | 3 | 182 | 3 | '-' -> '+' | arith_swap\nzzz.R_zzz.R_024.R | R/zzz.R | SURVIVED | 17 | 7 | 17 | 24 | 'length(field) > 0L' -> '!length(field) > 0L' | cond_negate\nrccValidators.R_rccValidators.R_003.R | R/rccValidators.R | KILLED | 6 | 28 | 6 | 50 | '^[.]*' -> 'NULL' | const_null\n030.setMethodS3.R_030.setMethodS3.R_026.R | R/030.setMethodS3.R | SURVIVED | 81 | 5 | 81 | 60 | 'Cannot set S3 method. Argument 'class' is empty.' -> 'NA_character_' | na_replace\n000.R_000.R_062.R | R/000.R | SURVIVED | 63 | 9 | 63 | 26 | '<unknown>' -> 'NA_character_' | na_replace"
    }

# mutation results for forcats remain stable

    {
      "outcome": "OK",
      "summary": "generated=852 | tested=10 | killed=9 | hanged=0 | survived=1 | mutation_score=90 | mutation_score_ci=59.5849973204762,98.2123786904927 | confidence=0.95",
      "mutants": "lvls.R_lvls.R_064.R | R/lvls.R | KILLED | 105 | 3 | 113 | 3 | '&&' -> '||' | logic_swap\nfct.R_fct.R_003.R | R/fct.R | KILLED | 38 | 3 | 38 | 44 | 'TRUE' -> 'NA' | na_replace\nlump.R_lump.R_074.R | R/lump.R | KILLED | 153 | 3 | 153 | 39 | 'lvls_other' -> '<deleted>' | stmt_delete\nrelevel.R_relevel.R_009.R | R/relevel.R | KILLED | 50 | 7 | 50 | 23 | '==' -> '!=' | rel_swap\nlump.R_lump.R_033.R | R/lump.R | KILLED | 118 | 3 | 122 | 3 | 'if' -> '<deleted>' | stmt_delete\nrecode.R_recode.R_043.R | R/recode.R | KILLED | 67 | 3 | 67 | 64 | '<-' -> '<deleted>' | stmt_delete\nlump.R_lump.R_108.R | R/lump.R | SURVIVED | 195 | 3 | 195 | 15 | '+' -> '-' | arith_swap\nlump.R_lump.R_020.R | R/lump.R | KILLED | 103 | 3 | 103 | 44 | 'TRUE' -> 'FALSE' | bool_flip\nlump.R_lump.R_138.R | R/lump.R | KILLED | 228 | 10 | 228 | 14 | '<' -> '>' | rel_swap\nlvls.R_lvls.R_037.R | R/lvls.R | KILLED | 58 | 3 | 70 | 3 | 'out' -> '<deleted>' | stmt_delete"
    }

# mutation results for jsonlite remain stable

    {
      "outcome": "OK",
      "summary": "generated=2662 | tested=10 | killed=1 | hanged=0 | survived=9 | mutation_score=10 | mutation_score_ci=1.78762130950729,40.4150026795238 | confidence=0.95",
      "mutants": "asJSON.character.R_asJSON.character.R_010.R | R/asJSON.character.R | KILLED | 7 | 3 | 10 | 3 | 'isTRUE(keep_vec_names) && length(names(x))' -> '!(isTRUE(keep_vec_names) && length(names(x)))' | cond_negate\ndeparse_vector.R_deparse_vector.R_064.R | R/deparse_vector.R | SURVIVED | 14 | 3 | 14 | 41 | 'TRUE' -> 'FALSE' | bool_flip\nunescape_unicode.R_unescape_unicode.R_038.R | R/unescape_unicode.R | SURVIVED | 13 | 41 | 13 | 61 | '\"' -> 'NA_character_' | na_replace\nflatten.R_flatten.R_011.R | R/flatten.R | SURVIVED | 33 | 3 | 35 | 3 | 'return' -> '<deleted>' | stmt_delete\nfromJSON.R_fromJSON.R_013.R | R/fromJSON.R | SURVIVED | 81 | 5 | 81 | 62 | 'Argument 'txt' must be a JSON string, URL or file.' -> 'NA_character_' | na_replace\nstream.R_stream.R_143.R | R/stream.R | SURVIVED | 213 | 3 | 213 | 92 | 'is.character(prefix) && length(prefix)' -> 'length(prefix)' | and_or_operand\napply_by_pages.R_apply_by_pages.R_062.R | R/apply_by_pages.R | SURVIVED | 27 | 38 | 27 | 47 | 'nrow' -> 'ncol' | fun_swap\nasJSON.sf.R_asJSON.sf.R_025.R | R/asJSON.sf.R | SURVIVED | 11 | 5 | 11 | 88 | 'FALSE' -> 'NA' | na_replace\nstream.R_stream.R_001.R | R/stream.R | SURVIVED | 122 | 1 | 176 | 1 | 'TRUE' -> 'FALSE' | bool_flip\nprettify.R_prettify.R_021.R | R/prettify.R | SURVIVED | 20 | 3 | 20 | 36 | 'TRUE' -> 'FALSE' | bool_flip"
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
      "summary": "generated=3621 | tested=10 | killed=8 | hanged=0 | survived=2 | mutation_score=80 | mutation_score_ci=49.0162471536642,94.3317848545625 | confidence=0.95",
      "mutants": "nanoduration.R_nanoduration.R_361.R | R/nanoduration.R | KILLED | 329 | 15 | 329 | 84 | 'nanotime' -> 'NULL' | const_null\nnanotime.R_nanotime.R_641.R | R/nanotime.R | SURVIVED | 643 | 15 | 643 | 55 | '|' -> '&' | logic_swap\nnanotime.R_nanotime.R_739.R | R/nanotime.R | KILLED | 727 | 9 | 730 | 9 | 'warning' -> '<deleted>' | stmt_delete\nnanotime.R_nanotime.R_036.R | R/nanotime.R | KILLED | 149 | 9 | 149 | 14 | '==' -> '!=' | rel_swap\nnanoival.R_nanoival.R_251.R | R/nanoival.R | KILLED | 328 | 1 | 331 | 12 | '!=' -> 'NULL' | const_null\nnanoperiod.R_nanoperiod.R_784.R | R/nanoperiod.R | KILLED | 794 | 1 | 796 | 2 | 'nanoperiod' -> 'NULL' | const_null\nnanoival.R_nanoival.R_326.R | R/nanoival.R | KILLED | 383 | 15 | 383 | 57 | 'new' -> '<deleted>' | stmt_delete\nnanotime.R_nanotime.R_685.R | R/nanotime.R | KILLED | 714 | 5 | 716 | 5 | 'if' -> '<deleted>' | stmt_delete\nnanoival.R_nanoival.R_356.R | R/nanoival.R | KILLED | 405 | 1 | 408 | 12 | '+' -> 'NA_character_' | na_replace\nnanoperiod.R_nanoperiod.R_616.R | R/nanoperiod.R | SURVIVED | 628 | 1 | 631 | 12 | 'ANY' -> 'NULL' | const_null"
    }

# mutation results for oRaklE remain stable

    {
      "outcome": "OK",
      "summary": "generated=10409 | tested=10 | killed=0 | hanged=0 | survived=10 | mutation_score=0 | mutation_score_ci=2.77555756156289e-15,27.7532799862889 | confidence=0.95",
      "mutants": "combine_models.R_combine_models.R_220.R | R/combine_models.R | SURVIVED | 109 | 7 | 109 | 111 | '\nPlease run the function again.' -> 'NULL' | const_null\nlong_term_future.R_long_term_future.R_126.R | R/long_term_future.R | SURVIVED | 51 | 3 | 87 | 3 | 'if' -> '<deleted>' | stmt_delete\ncombine_models_future.R_combine_models_future.R_234.R | R/combine_models_future.R | SURVIVED | 116 | 3 | 116 | 63 | '8' -> 'NA_real_' | na_replace\nget_historic_load_data.R_get_historic_load_data.R_288.R | R/get_historic_load_data.R | SURVIVED | 134 | 9 | 134 | 30 | '<=' -> '>=' | rel_swap\nlong_term_lm.R_long_term_lm.R_185.R | R/long_term_lm.R | SURVIVED | 110 | 13 | 110 | 53 | '\n' -> 'NA_character_' | na_replace\nlong_term_future.R_long_term_future.R_649.R | R/long_term_future.R | SURVIVED | 248 | 3 | 248 | 59 | '<-' -> '<deleted>' | stmt_delete\nfill_missing_data.R_fill_missing_data.R_162.R | R/fill_missing_data.R | SURVIVED | 112 | 5 | 112 | 61 | 'all' -> 'any' | fun_swap\nmid_term_future.R_mid_term_future.R_057.R | R/mid_term_future.R | SURVIVED | 44 | 30 | 46 | 7 | 'seasonal_avg_hourly_demand' -> 'NULL' | const_null\ndecompose_load_data.R_decompose_load_data.R_590.R | R/decompose_load_data.R | SURVIVED | 242 | 3 | 252 | 3 | '+' -> '-' | arith_swap\ncombine_models.R_combine_models.R_688.R | R/combine_models.R | SURVIVED | 280 | 3 | 314 | 41 | '+' -> '-' | arith_swap"
    }

# mutation results for prettyunits remain stable

    {
      "outcome": "OK",
      "summary": "generated=1144 | tested=10 | killed=10 | hanged=0 | survived=0 | mutation_score=100 | mutation_score_ci=72.2467200137111,100 | confidence=0.95",
      "mutants": "rounding.R_rounding.R_032.R | R/rounding.R | KILLED | 34 | 9 | 34 | 17 | '0' -> 'NULL' | const_null\nsizes.R_sizes.R_076.R | R/sizes.R | KILLED | 46 | 5 | 46 | 24 | '==' -> '!=' | rel_swap\nsizes.R_sizes.R_037.R | R/sizes.R | KILLED | 27 | 5 | 27 | 72 | '1000' -> 'NULL' | const_null\nrounding.R_rounding.R_005.R | R/rounding.R | KILLED | 19 | 7 | 19 | 25 | '1' -> 'NULL' | const_null\nnumbers.R_numbers.R_018.R | R/numbers.R | KILLED | 17 | 5 | 17 | 94 | 'u' -> 'NA_character_' | na_replace\ntime-ago.R_time-ago.R_250.R | R/time-ago.R | KILLED | 100 | 3 | 107 | 3 | 'structure' -> '<deleted>' | stmt_delete\nnumbers.R_numbers.R_223.R | R/numbers.R | KILLED | 101 | 5 | 101 | 54 | '10' -> 'NULL' | const_null\ntime-ago.R_time-ago.R_201.R | R/time-ago.R | KILLED | 65 | 3 | 74 | 3 | '<-' -> '<deleted>' | stmt_delete\ntime-ago.R_time-ago.R_101.R | R/time-ago.R | KILLED | 37 | 3 | 48 | 3 | '%d min' -> 'NULL' | const_null\nsizes.R_sizes.R_039.R | R/sizes.R | KILLED | 27 | 5 | 27 | 72 | '-' -> '+' | arith_swap"
    }

# mutation results for scales remain stable

    {
      "outcome": "OK",
      "summary": "generated=5144 | tested=10 | killed=7 | hanged=0 | survived=3 | mutation_score=70 | mutation_score_ci=39.6778147461145,89.2208732593699 | confidence=0.95",
      "mutants": "breaks-log.R_breaks-log.R_191.R | R/breaks-log.R | SURVIVED | 140 | 7 | 140 | 30 | '10' -> 'NULL' | const_null\npal-.R_pal-.R_002.R | R/pal-.R | KILLED | 153 | 3 | 153 | 68 | '<-' -> '<deleted>' | stmt_delete\ncolour-mapping.R_colour-mapping.R_304.R | R/colour-mapping.R | KILLED | 334 | 45 | 334 | 62 | '1' -> 'NA_real_' | na_replace\npal-.R_pal-.R_046.R | R/pal-.R | KILLED | 192 | 3 | 192 | 74 | 'is_pal(x) && any(palette_type(x) %in% c(\"numeric\", \"double\", ' -> 'is_pal(x)' | and_or_operand\nlabel-number.R_label-number.R_091.R | R/label-number.R | KILLED | 308 | 3 | 308 | 24 | '0' -> 'NA_real_' | na_replace\ntransform-compose.R_transform-compose.R_036.R | R/transform-compose.R | SURVIVED | 33 | 7 | 33 | 11 | 'break' -> 'next' | loop_ctrl\nutils.R_utils.R_085.R | R/utils.R | KILLED | 79 | 16 | 79 | 24 | 'n != size' -> '!n != size' | cond_negate\nutils.R_utils.R_036.R | R/utils.R | SURVIVED | 16 | 23 | 16 | 40 | '1' -> 'NULL' | const_null\nlabel-ordinal.R_label-ordinal.R_062.R | R/label-ordinal.R | KILLED | 115 | 5 | 122 | 5 | '' -> 'NA_character_' | na_replace\nlabel-number.R_label-number.R_261.R | R/label-number.R | KILLED | 450 | 3 | 450 | 71 | '<-' -> '<deleted>' | stmt_delete"
    }

# mutation results for stringr remain stable

    {
      "outcome": "OK",
      "summary": "generated=1408 | tested=10 | killed=3 | hanged=0 | survived=7 | mutation_score=30 | mutation_score_ci=10.7791267406301,60.3221852538855 | confidence=0.95",
      "mutants": "interp.R_interp.R_130.R | R/interp.R | SURVIVED | 249 | 3 | 249 | 67 | 'min' -> 'max' | fun_swap\nview.R_view.R_131.R | R/view.R | KILLED | 165 | 3 | 169 | 3 | '<-' -> '<deleted>' | stmt_delete\nview.R_view.R_229.R | R/view.R | SURVIVED | 210 | 5 | 210 | 49 | ' more\n' -> 'NULL' | const_null\nmodifiers.R_modifiers.R_103.R | R/modifiers.R | SURVIVED | 219 | 3 | 224 | 3 | 'if' -> '<deleted>' | stmt_delete\nview.R_view.R_188.R | R/view.R | SURVIVED | 200 | 16 | 200 | 47 | '[' -> 'NULL' | const_null\nmodifiers.R_modifiers.R_064.R | R/modifiers.R | KILLED | 173 | 7 | 173 | 35 | 'NA' -> 'NULL' | const_null\nword.R_word.R_026.R | R/word.R | SURVIVED | 41 | 28 | 41 | 35 | '<' -> '<=' | rel_boundary\nview.R_view.R_175.R | R/view.R | SURVIVED | 193 | 3 | 196 | 3 | 'cli::cli_inform' -> '<deleted>' | stmt_delete\nwrap.R_wrap.R_001.R | R/wrap.R | SURVIVED | 27 | 1 | 52 | 1 | 'TRUE' -> 'FALSE' | bool_flip\ninterp.R_interp.R_103.R | R/interp.R | KILLED | 215 | 3 | 215 | 56 | '\\$(\\[(.+?)\\])?.*' -> 'NA_character_' | na_replace"
    }

