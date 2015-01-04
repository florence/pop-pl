#lang racket/signature

;; -> Network
new-network
;; Network module-path? -> (Listof Messages)
spawn-actor!
;; Network Message -> (Listof Messages
send-message!
