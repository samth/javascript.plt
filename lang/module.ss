#lang scheme/base

(require "lang.ss")

(provide (rename-out [module-begin #%module-begin])
         (except-out (all-from-out scheme/base) #%module-begin))
