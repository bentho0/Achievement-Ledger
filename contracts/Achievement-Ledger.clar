;; File: achievement-ledger.clar
;; A personal and team achievement tracking system for recording accomplishments.
(define-map achievements
  { participant: principal }
  { points: uint, target: uint, completion: uint })

;; Event definitions
(define-constant POINTS_EARNED_EVENT "points-earned")
(define-constant TARGET_SET_EVENT "target-defined")
(define-constant ACHIEVEMENT_UNLOCKED_EVENT "achievement-unlocked")

(define-map event-logs
  { type: (string-ascii 25), participant: principal }
  { value: uint })

;; Private helper functions to emit custom events. The print function simulates event emission.
(define-private (emit-points (participant principal) (value uint))
    (begin
        (map-insert event-logs { type: POINTS_EARNED_EVENT, participant: participant } { value: value })
        (print { event: POINTS_EARNED_EVENT, participant: participant, value: value })
        (ok true)))

;; Allow users to record points toward their achievement.
(define-public (record-points (value uint))
    (let ((current-achievement (map-get? achievements { participant: tx-sender })))
        (if (is-some current-achievement)
            ;; Update existing points record
            (let ((achievement-data (unwrap! current-achievement (err "Missing record")))
                  (current-points (get points achievement-data))
                  (current-target (get target achievement-data))
                  (current-completion (get completion achievement-data))
                  (new-points (+ current-points value)))
                (begin
                    (map-set achievements { participant: tx-sender }
                        { points: new-points, target: current-target, completion: current-completion })
                    (asserts! (is-ok (emit-points tx-sender value)) (err "Failed to emit points event"))
                    (ok value)))
            (begin
                (print { type: "achievement-created", participant: tx-sender })
                (map-set achievements { participant: tx-sender }
                    { points: value, target: u0, completion: u0 })
                (asserts! (is-ok (emit-points tx-sender value)) (err "Failed to emit points event"))
                (ok value)))))

;; Set a target for participant achievement.
(define-public (set-target (target uint))
    (let ((current-achievement (map-get? achievements { participant: tx-sender })))
        (if (is-some current-achievement)
            (let ((achievement-data (unwrap! current-achievement (err "Achievement data not found")))
                  (current-points (get points achievement-data))
                  (current-completion (get completion achievement-data)))
                (begin
                    (asserts! (> target u0) (err "Target must be greater than zero"))
                    (map-set achievements { participant: tx-sender }
                        { points: current-points, target: target, completion: current-completion })
                    (print { event: TARGET_SET_EVENT, participant: tx-sender, target: target })
                    (ok target)))
            (err "Achievement record not found"))))

;; Check if the participant has reached their achievement target.
(define-public (verify-achievement)
    (let ((current-achievement (map-get? achievements { participant: tx-sender })))
        (if (is-some current-achievement)
            (let ((achievement-data (unwrap! current-achievement (err "Achievement data not found")))
                  (points (get points achievement-data))
                  (target (get target achievement-data)))
                (if (>= points target)
                    (begin
                        (print { event: ACHIEVEMENT_UNLOCKED_EVENT, participant: tx-sender, target: target })
                        (ok { status: "Unlocked", target: target, points: points }))
                    (ok { status: "In Progress", target: target, points: points })))
            (err "Achievement record not found"))))