#lang racket/signature

;; -> Network
new-network
;; Network (or module-path? prescription^) -> (Listof Messages)
spawn-actor!
;; Network Message -> (Listof Messages)
send-message!
;; Network Time -> (Listof Messages)
advance!
