;chezmbe.scm
;r4rs-syntax mbe for Chez Scheme (only top-level macros supported)
;Dorai Sitaram

(extend-syntax (define-syntax syntax-rules)
  ((define-syntax m (syntax-rules kk . cc))
   (extend-syntax (m . kk) . cc)))
