# mutation results for jsonlite remain stable

    {
      "outcome": "OK",
      "summary": "generated=2667 | tested=10 | killed=2 | hanged=0 | survived=8 | mutation_score=20 | mutation_score_ci=5.66821514543752,50.9837528463358 | confidence=0.95",
      "mutants": "asJSON.character.R_asJSON.character.R_010.R | R/asJSON.character.R | KILLED | 7 | 3 | 10 | 3 | 'isTRUE(keep_vec_names) && length(names(x))' -> '!(isTRUE(keep_vec_names) && length(names(x)))' | cond_negate\nwarn_keep_vec_names.R_warn_keep_vec_names.R_006.R | R/warn_keep_vec_names.R | SURVIVED | 2 | 3 | 2 | 295 | 'and named vectors will be translated into arrays instead of objects. ' -> 'NA_character_' | na_replace\ndeparse_vector.R_deparse_vector.R_063.R | R/deparse_vector.R | SURVIVED | 14 | 3 | 14 | 41 | 'TRUE' -> 'NA' | na_replace\nunescape_unicode.R_unescape_unicode.R_033.R | R/unescape_unicode.R | SURVIVED | 13 | 9 | 13 | 99 | '\\' -> 'NULL' | const_null\nflatten.R_flatten.R_010.R | R/flatten.R | SURVIVED | 33 | 7 | 33 | 21 | 'any' -> 'all' | fun_swap\nfromJSON.R_fromJSON.R_012.R | R/fromJSON.R | SURVIVED | 80 | 3 | 82 | 3 | 'stop' -> '<deleted>' | stmt_delete\nstream.R_stream.R_138.R | R/stream.R | SURVIVED | 213 | 3 | 213 | 92 | '&&' -> '||' | logic_swap\napply_by_pages.R_apply_by_pages.R_062.R | R/apply_by_pages.R | SURVIVED | 27 | 38 | 27 | 47 | 'nrow' -> 'ncol' | fun_swap\nasJSON.sf.R_asJSON.sf.R_025.R | R/asJSON.sf.R | KILLED | 11 | 5 | 11 | 88 | 'FALSE' -> 'NA' | na_replace\nstop.R_stop.R_001.R | R/stop.R | SURVIVED | 1 | 1 | 3 | 1 | 'FALSE' -> 'TRUE' | bool_flip"
    }

