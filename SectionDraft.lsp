;;; ============================================================
;;; SectionDraft.lsp  V89 (自適應邊界狙擊雷達版)
;;; 核心進化：實裝「動態邊界追蹤雷達」。
;;; 1. 解決長開口漏牆問題：當切到門窗時，動態讀取其 Y 軸極限邊界 (Y_min, Y_max)。
;;; 2. 精準狙擊：直接在開口上下邊緣 ±5cm 處生成動態射線，無論開口多長皆可完美捕捉連接牆體。
;;; 3. 擴大選擇框 (ssget) 至 ±1500cm，確保遠距牆線完整載入記憶體。
;;; 指令: SECDRAFT
;;; ============================================================
(vl-load-com)

;; --- 全域記憶體初始化 ---
(if (null *sd-wall-layers*) (setq *sd-wall-layers* nil))
(if (null *sd-balc-layers*) (setq *sd-balc-layers* nil))
(if (null *sd-door-layers*) (setq *sd-door-layers* nil))
(if (null *sd-rel-heights*) (setq *sd-rel-heights* '()))
(if (null *sd-slab-thk*)    (setq *sd-slab-thk* 15.0))
(if (null *sd-para-t*)      (setq *sd-para-t* 15.0))
(if (null *sd-para-h*)      (setq *sd-para-h* 110.0))
(if (null *sd-draw-plas*)   (setq *sd-draw-plas* "Y"))
(if (null *sd-plas-thk*)    (setq *sd-plas-thk* 2.0))
(if (null *sd-void-ans*)    (setq *sd-void-ans* "N"))
(if (null *sd-sort-dir*)    (setq *sd-sort-dir* "L"))

;; --- 基礎函式庫 ---
(defun sd:error (msg)
  (if doc (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
  (if (and msg (not (wcmatch (strcase msg t) "*BREAK*,*CANCEL*,*EXIT*")))
    (princ (strcat "\n錯誤: " msg))
  )
  (if old-clayer (vl-catch-all-apply 'setvar (list "CLAYER" old-clayer)))
  (if old-err (setq *error* old-err))
  (princ "\n[指令已安全中斷]")
  (princ)
)

(defun sd:ensure-layer (name color / )
  (if (not (tblsearch "LAYER" name))
    (command "_.LAYER" "_N" name "_C" (itoa color) name "")
  )
)

(defun sd:line-pts (ename / ed)
  (setq ed (entget ename))
  (list (cdr (assoc 10 ed)) (cdr (assoc 11 ed)))
)

(defun sd:line-mid (ename / ed p1 p2)
  (setq ed (entget ename) p1 (cdr (assoc 10 ed)) p2 (cdr (assoc 11 ed)))
  (list (/ (+ (car p1) (car p2)) 2.0) (/ (+ (cadr p1) (cadr p2)) 2.0))
)

(defun sd:get-bbox (ename / obj minpt maxpt)
  (setq obj (vlax-ename->vla-object ename))
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'minpt 'maxpt))))
    (list (vlax-safearray->list minpt) (vlax-safearray->list maxpt))
    nil
  )
)

(defun sd:intersect-ray (ray-obj target-obj / pts sa pts-list lst i)
  (setq lst '())
  (if (not (vl-catch-all-error-p (setq pts (vl-catch-all-apply 'vla-IntersectWith (list ray-obj target-obj acExtendNone)))))
    (if (and pts (= (type pts) 'variant) (>= (vlax-variant-type pts) 8192))
      (progn
        (setq sa (vlax-variant-value pts))
        (if (not (vl-catch-all-error-p (setq pts-list (vl-catch-all-apply 'vlax-safearray->list (list sa)))))
          (progn
            (setq i 0)
            (while (< i (length pts-list))
              (setq lst (cons (nth i pts-list) lst))
              (setq i (+ i 3))
            )
          )
        )
      )
    )
  )
  lst
)

(defun sd:remove-duplicates-tol (lst tol / final-lst last-x)
  (setq lst (vl-sort lst '<) final-lst '() last-x -1e10)
  (foreach x lst
    (if (> (abs (- x last-x)) tol)
      (progn (setq final-lst (append final-lst (list x))) (setq last-x x))
    )
  )
  final-lst
)

(defun sd:pair-walls (x-list default-thk / i j dx result paired)
  (setq x-list (vl-sort x-list '<) result '() i 0)
  (while (< i (length x-list))
    (setq paired nil j (1+ i))
    (while (and (not paired) (< j (length x-list)) (<= (- (nth j x-list) (nth i x-list)) 80.0))
      (setq dx (- (nth j x-list) (nth i x-list)))
      (if (and (>= dx 8.0) (<= dx 80.0))
        (progn 
          (setq result (append result (list (list (nth i x-list) (nth j x-list))))) 
          (setq i j paired T)
        )
      )
      (setq j (1+ j))
    )
    (if (not paired)
      (setq result (append result (list (list (nth i x-list) (+ (nth i x-list) default-thk)))))
    )
    (setq i (1+ i))
  )
  result
)

(defun sd:merge-walls (pairs / sorted merged curr next i)
  (if (not pairs) 
    nil
    (progn
      (setq sorted (vl-sort pairs (function (lambda (a b) (< (car a) (car b))))))
      (setq merged (list (car sorted)))
      (setq i 1)
      (while (< i (length sorted))
        (setq curr (last merged) next (nth i sorted))
        (if (and (<= (car next) (+ (cadr curr) 2.0)) (= (caddr curr) (caddr next)))
          (setq merged (subst (list (car curr) (max (cadr curr) (cadr next)) (caddr curr)) curr merged))
          (setq merged (append merged (list next)))
        )
        (setq i (1+ i))
      )
      merged
    )
  )
)

(defun sd:extract-layer-list (ss / i lay lay-list)
  (setq i 0 lay-list '())
  (if ss
    (while (< i (sslength ss))
      (setq lay (cdr (assoc 8 (entget (ssname ss i)))))
      (if (not (member lay lay-list)) (setq lay-list (cons lay lay-list)))
      (setq i (1+ i))
    )
  )
  lay-list
)

(defun sd:join-layers (lay-list / str)
  (setq str "")
  (foreach lay lay-list 
    (if (= str "") (setq str lay) (setq str (strcat str "," lay)))
  )
  str
)

(defun sd:snap-to-closest-wall (x walls / best-x min-dist d)
  (setq best-x x min-dist 20.0)
  (foreach w walls
    (setq d (abs (- (car w) x)))
    (if (<= d min-dist) (setq min-dist d best-x (car w)))
    (setq d (abs (- (cadr w) x)))
    (if (<= d min-dist) (setq min-dist d best-x (cadr w)))
  )
  best-x
)

(defun sd:draw-sweep (y-val layer-name start-x end-x original-obs p-thickness / cx obs draw-end obs-list raw-left exp-right)
  (setvar "CLAYER" layer-name)
  (setq obs-list '())
  (foreach obs original-obs
    (if (and obs (listp obs) (>= (length obs) 2))
      (setq obs-list (cons (list (car obs) (cadr obs) (- (car obs) p-thickness) (+ (cadr obs) p-thickness)) obs-list))
    )
  )
  (setq obs-list (vl-sort obs-list (function (lambda (a b) (< (car a) (car b))))))
  (setq cx start-x)
  (foreach obs obs-list
    (setq raw-left (car obs) exp-right (cadddr obs))
    (if (and (> raw-left (+ cx 0.1)) (< cx end-x))
      (progn
        (setq draw-end (min end-x raw-left))
        (command "_.LINE" "_NON" (list cx y-val 0) "_NON" (list draw-end y-val 0) "")
      )
    )
    (setq cx (max cx exp-right))
  )
  (if (< (+ cx 0.1) end-x)
    (command "_.LINE" "_NON" (list cx y-val 0) "_NON" (list end-x y-val 0) "")
  )
)

;;; --- 主程式 ---
(defun c:SECDRAFT ( / 
  acadObj doc old-err old-clayer base-y h-count sec-ss temp-dir sort-dir sec-ents
  lay-wall-list lay-balc-list lay-door-list lay-filter-str
  rel-heights heights acc def-h temp void-ans void-ranges keep-going ent1 ent2 bbox1 bbox2 pt-left pt-right
  f sec-ent sec-pts sec-y ray0 narrow-rays wide-rays pt1 pt2 box-pt1 box-pt2 search-ss
  i ename obj lay typ bbox px raw-wall-x r d-int near-door ymin ymax door-y-bounds
  door-intervals wall-x-list balc-x-list wp bp current-floor-walls is-wall is-door-line is-balc
  floor-walls-list floor-doors-list curr-walls prev-walls y-bottom y-next y-ceil slab-x1 slab-x2 ext-min ext-max
  all-voids rv-s rv-e slab-obs-top slab-obs-bot exp-curr exp-prev exp-rf-bot
  w w-x1 w-x2 w-type is-door is-void-on-left is-void-on-right is-outer-left is-outer-right
  w-y-bot w-y-top p-bot-l p-bot-r p-top-l p-top-r top-plaster-x1 top-plaster-x2 rf-walls valid-rf-walls rf-min rf-max check-walls
  draft-layer slab-layer opening-layer floor-layer plaster-layer dim-layer
  )

  (setq acadObj (vlax-get-acad-object) doc (vla-get-ActiveDocument acadObj))
  (setq old-err *error* *error* sd:error old-clayer (getvar "CLAYER"))

  (princ "\n=== SectionDraft V89 (自適應邊界狙擊雷達版) ===")

  (setq draft-layer "A-WALL-SECT" slab-layer "A-FLOR-SECT" opening-layer "A-OPEN" floor-layer "A-FLOR-LEVL" plaster-layer "A-WALL-FINI" dim-layer "A-DIM")   
  (sd:ensure-layer draft-layer 4) (sd:ensure-layer slab-layer 4) (sd:ensure-layer opening-layer 2) (sd:ensure-layer floor-layer 8) (sd:ensure-layer plaster-layer 8) (sd:ensure-layer dim-layer 7)

  (setq keep-going T)
  (while keep-going
    (setq ins-pt (getpoint "\n[1] 請點選地板起點 (1F高度): "))
    (if ins-pt (setq base-y (cadr ins-pt) keep-going nil) (princ "\n⚠️ [警告] 必須點選起點！"))
  )

  (setq lay-wall-list nil keep-going T)
  (while keep-going
    (if *sd-wall-layers* (princ (strcat "\n[2] 實心牆樣本 [記憶: " (sd:join-layers *sd-wall-layers*) "] (Enter沿用): "))
      (princ "\n[2] 請框選「實心牆」樣本 (必選): ")
    )
    (setq wall-ss-pick (vl-catch-all-apply 'ssget))
    (cond
      ((and (not (vl-catch-all-error-p wall-ss-pick)) wall-ss-pick)
       (setq *sd-wall-layers* (sd:extract-layer-list wall-ss-pick) lay-wall-list *sd-wall-layers* keep-going nil))
      (*sd-wall-layers* (setq lay-wall-list *sd-wall-layers* keep-going nil) (princ " -> [套用記憶]"))
      (T (princ "\n⚠️ 尚未選取牆圖層！"))
    )
  )

  (setq lay-balc-list nil keep-going T)
  (while keep-going
    (if *sd-balc-layers* (princ (strcat "\n[3] 陽台/女兒牆樣本 [記憶: " (sd:join-layers *sd-balc-layers*) "] (Enter沿用): "))
      (princ "\n[3] 請框選「陽台/女兒牆」樣本 (Enter跳過): ")
    )
    (setq balc-ss-pick (vl-catch-all-apply 'ssget))
    (cond
      ((and (not (vl-catch-all-error-p balc-ss-pick)) balc-ss-pick)
       (setq *sd-balc-layers* (sd:extract-layer-list balc-ss-pick) lay-balc-list *sd-balc-layers* keep-going nil))
      (*sd-balc-layers* (setq lay-balc-list *sd-balc-layers* keep-going nil) (princ " -> [套用記憶]"))
      (T (setq lay-balc-list '() keep-going nil) (princ " -> [跳過]"))
    )
  )

  (setq lay-door-list nil keep-going T)
  (while keep-going
    (if *sd-door-layers* (princ (strcat "\n[4] 門窗/開口樣本 [記憶: " (sd:join-layers *sd-door-layers*) "] (Enter沿用): "))
      (princ "\n[4] 請框選「門窗/開口」樣本 (Enter跳過): ")
    )
    (setq door-ss-pick (vl-catch-all-apply 'ssget))
    (cond
      ((and (not (vl-catch-all-error-p door-ss-pick)) door-ss-pick)
       (setq *sd-door-layers* (sd:extract-layer-list door-ss-pick) lay-door-list *sd-door-layers* keep-going nil))
      (*sd-door-layers* (setq lay-door-list *sd-door-layers* keep-going nil) (princ " -> [套用記憶]"))
      (T (setq lay-door-list '() keep-going nil) (princ " -> [跳過]"))
    )
  )

  (setq sec-ss nil keep-going T)
  (while keep-going
    (princ "\n[5] 請框選所有平面圖的剖切紅線 (必選): ")
    (setq sec-ss (vl-catch-all-apply 'ssget '(((0 . "LINE")))))
    (if (and (not (vl-catch-all-error-p sec-ss)) sec-ss)
      (setq keep-going nil) (princ "\n⚠️ 尚未選取紅線！")
    )
  )
  (setq h-count (sslength sec-ss))
  
  (initget "L B T")
  (setq temp-dir (getkword (strcat "\n -> 共 " (itoa h-count) " 樓層。排序方向 [由左至右(L)/由下至上(B)/由上至下(T)] <" *sd-sort-dir* ">: ")))
  (if temp-dir (setq *sd-sort-dir* temp-dir))
  (setq sort-dir *sd-sort-dir*)

  (setq sec-ents '() i 0)
  (while (< i h-count) (setq sec-ents (cons (ssname sec-ss i) sec-ents)) (setq i (1+ i)))
  (setq sec-ents (vl-sort sec-ents (function (lambda (e1 e2) (setq pt1 (sd:line-mid e1) pt2 (sd:line-mid e2)) (cond ((= sort-dir "L") (< (car pt1) (car pt2))) ((= sort-dir "B") (< (cadr pt1) (cadr pt2))) ((= sort-dir "T") (> (cadr pt1) (cadr pt2))) (T (< (car pt1) (car pt2))))))))

  (setq rel-heights '() i 1)
  (while (< i h-count)
    (setq def-h (if (>= (length *sd-rel-heights*) i) (nth (1- i) *sd-rel-heights*) 320.0))
    (setq h-val (getreal (strcat "\n -> 第 " (itoa i) " 層高度 <" (rtos def-h 2 1) ">: ")))
    (if (null h-val) (setq h-val def-h))
    (setq rel-heights (append rel-heights (list h-val)))
    (setq i (1+ i))
  )
  (setq *sd-rel-heights* rel-heights)
  (setq heights '(0.0) acc 0.0)
  (foreach h rel-heights (setq acc (+ acc h)) (setq heights (append heights (list acc))))

  (setq temp (getreal (strcat "\n[8] 樓板厚度 <" (rtos *sd-slab-thk* 2 1) ">: "))) (if temp (setq *sd-slab-thk* temp)) (setq slab-thick *sd-slab-thk*)
  (setq temp (getreal (strcat "\n[9] 女兒牆厚度 <" (rtos *sd-para-t* 2 1) ">: "))) (if temp (setq *sd-para-t* temp)) (setq parapet-t *sd-para-t*)
  (setq temp (getreal (strcat "\n[10] 女兒牆高度 <" (rtos *sd-para-h* 2 1) ">: "))) (if temp (setq *sd-para-h* temp)) (setq parapet-h *sd-para-h*)
  (setq temp (getstring (strcat "\n[11] 自動粉刷線？(Y/N) <" *sd-draw-plas* ">: "))) (if (and (/= temp "") (/= temp nil)) (setq *sd-draw-plas* (strcase temp)))
  (setq draw-plaster (if (or (= *sd-draw-plas* "N") (= *sd-draw-plas* "NO")) nil T))
  (if draw-plaster (progn (setq temp (getreal (strcat "\n[12] 粉刷線厚度 <" (rtos *sd-plas-thk* 2 1) ">: "))) (if temp (setq *sd-plas-thk* temp)) (setq p-thk *sd-plas-thk*)) (setq p-thk 0.0))

  (setq void-ranges '())
  (initget "Y N")
  (setq void-ans (getkword (strcat "\n[13] 處理樓梯/天井範圍？(Y/N) <" *sd-void-ans* ">: ")))
  (if (null void-ans) (setq void-ans *sd-void-ans*))
  (setq *sd-void-ans* void-ans)

  (if (= void-ans "Y")
    (progn
      (setq keep-going T)
      (while keep-going
        (setq ent1 (car (vl-catch-all-apply 'entsel (list "\n請選取天井「左側」邊界線 (Enter 結束): "))))
        (if (null ent1) (setq keep-going nil)
          (progn
            (setq ent2 (car (vl-catch-all-apply 'entsel (list "\n請選取天井「右側」邊界線 (Enter 結束): "))))
            (if (null ent2) (setq keep-going nil)
              (progn
                (setq bbox1 (sd:get-bbox ent1) bbox2 (sd:get-bbox ent2))
                (if (and bbox1 bbox2)
                  (progn
                    (setq pt-left (car (car bbox1)) pt-right (car (car bbox2)))
                    (setq void-ranges (cons (list (min pt-left pt-right) (max pt-left pt-right)) void-ranges))
                    (princ (strcat " → 已記錄！共 " (itoa (length void-ranges)) " 組"))
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  (vla-StartUndoMark doc)

  ;; --- Phase 1: 平面圖特徵掃描 (自適應雷達版) ---
  (setq floor-walls-list '() floor-doors-list '())
  (setq f 0)
  (while (< f h-count)
    (setq sec-ent (nth f sec-ents) sec-pts (sd:line-pts sec-ent) sec-y (cadr (car sec-pts)))
    
    (setq pt1 (list (- (car (car sec-pts)) 1500.0) sec-y 0) pt2 (list (+ (car (cadr sec-pts)) 1500.0) sec-y 0))
    
    (entmake (list '(0 . "LINE") (cons 10 pt1) (cons 11 pt2)))
    (setq ray0 (vlax-ename->vla-object (entlast)))
    
    (setq narrow-rays '())
    (foreach dy '(2.0 -2.0)
      (entmake (list '(0 . "LINE") (cons 10 (list (car pt1) (+ sec-y dy) 0)) (cons 11 (list (car pt2) (+ sec-y dy) 0))))
      (setq narrow-rays (cons (vlax-ename->vla-object (entlast)) narrow-rays))
    )
    
    ;; ★ V89 關鍵：擴大搜索框到 ±1500cm，保證長窗戶的牆線被載入記憶體
    (setq box-pt1 (list (- (car pt1) 100) (- sec-y 1500.0)) box-pt2 (list (+ (car pt2) 100) (+ sec-y 1500.0)))
    (setq lay-filter-str (sd:join-layers (append lay-wall-list lay-balc-list lay-door-list)))
    (setq search-ss (ssget "_C" box-pt1 box-pt2 (list (cons 0 "LINE,*POLYLINE,INSERT,ARC") (cons 8 lay-filter-str))))
    
    (setq door-intervals '() door-y-bounds '() wall-x-list '() balc-x-list '())
    
    ;; Pass A: 找門窗，並記錄其 Y 軸極限 (Top & Bottom)
    (setq i 0)
    (if search-ss
      (while (< i (sslength search-ss))
        (setq ename (ssname search-ss i) obj (vlax-ename->vla-object ename) lay (cdr (assoc 8 (entget ename))))
        (if (and lay-door-list (member lay lay-door-list))
          (if (sd:intersect-ray ray0 obj)
            (progn
              (setq bbox (sd:get-bbox ename))
              (if bbox 
                (progn
                  (setq door-intervals (cons (list (- (car (car bbox)) 5.0) (+ (car (cadr bbox)) 5.0)) door-intervals))
                  ;; 記錄門窗的最上緣與最下緣
                  (setq door-y-bounds (cons (list (cadr (car bbox)) (cadr (cadr bbox))) door-y-bounds))
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
    
    ;; ★ V89 關鍵：根據門窗極限，動態生成狙擊射線 ★
    (setq wide-rays '())
    (foreach yb door-y-bounds
      (setq ymin (car yb) ymax (cadr yb))
      ;; 在邊緣內外 ±5cm 處開槍，精準擊中連接的實體牆
      (foreach dy '(-5.0 5.0)
        (entmake (list '(0 . "LINE") (cons 10 (list (car pt1) (+ ymin dy) 0)) (cons 11 (list (car pt2) (+ ymin dy) 0))))
        (setq wide-rays (cons (vlax-ename->vla-object (entlast)) wide-rays))
        (entmake (list '(0 . "LINE") (cons 10 (list (car pt1) (+ ymax dy) 0)) (cons 11 (list (car pt2) (+ ymax dy) 0))))
        (setq wide-rays (cons (vlax-ename->vla-object (entlast)) wide-rays))
      )
    )
    
    ;; Pass B: 實體牆壁抓取
    (setq i 0)
    (if search-ss
      (while (< i (sslength search-ss))
        (setq ename (ssname search-ss i) obj (vlax-ename->vla-object ename) lay (cdr (assoc 8 (entget ename))) typ (cdr (assoc 0 (entget ename))))
        
        (setq is-wall (member lay lay-wall-list))
        (setq is-balc (and lay-balc-list (member lay lay-balc-list)))
        (setq is-door-line (and lay-door-list (member lay lay-door-list) (wcmatch typ "LINE,*POLYLINE")))
        
        (if (or is-wall is-door-line)
          (progn
            (if (setq px (sd:intersect-ray ray0 obj)) (foreach x px (setq wall-x-list (cons x wall-x-list))))
            (foreach r narrow-rays (if (setq px (sd:intersect-ray r obj)) (foreach x px (setq wall-x-list (cons x wall-x-list)))))
            
            (if door-intervals
              (foreach r wide-rays
                (if (setq px (sd:intersect-ray r obj))
                  (foreach x px
                    (setq near-door nil)
                    (foreach d-int door-intervals
                      (if (and (>= x (- (car d-int) 15.0)) (<= x (+ (cadr d-int) 15.0))) (setq near-door T))
                    )
                    (if near-door (setq wall-x-list (cons x wall-x-list)))
                  )
                )
              )
            )
          )
        )
        
        (if is-balc
          (progn
            (if (setq px (sd:intersect-ray ray0 obj)) (foreach x px (setq balc-x-list (cons x balc-x-list))))
            (foreach r narrow-rays (if (setq px (sd:intersect-ray r obj)) (foreach x px (setq balc-x-list (cons x balc-x-list)))))
          )
        )
        
        (setq i (1+ i))
      )
    )
    
    (vla-Delete ray0)
    (foreach r narrow-rays (vla-Delete r))
    (foreach r wide-rays (vla-Delete r))
    
    (setq raw-wall-x (sd:remove-duplicates-tol wall-x-list 1.0))
    (setq wp (mapcar '(lambda (p) (append p (list 'WALL))) (sd:pair-walls raw-wall-x 15.0)))
    (setq bp (mapcar '(lambda (p) (append p (list 'PARAPET))) (sd:pair-walls (sd:remove-duplicates-tol balc-x-list 1.0) parapet-t)))
    (setq current-floor-walls (sd:merge-walls (append wp bp)))
    
    (princ (strcat "\n -> 樓層 " (itoa (1+ f)) ": 偵測到 " (itoa (length raw-wall-x)) " 個牆點，配對出 " (itoa (length current-floor-walls)) " 道牆體。"))
    (setq floor-walls-list (append floor-walls-list (list current-floor-walls)))
    (setq floor-doors-list (append floor-doors-list (list door-intervals)))
    (setq f (1+ f))
  )

  ;; --- Phase 2: 一般住宅層 ---
  (setq f 0)
  (while (< f (1- h-count))
    (setq curr-walls (nth f floor-walls-list) door-intervals (nth f floor-doors-list))
    (if curr-walls
      (progn
        (setq curr-min (car (car curr-walls)) curr-max (cadr (last curr-walls)))
        (if (> f 0) 
          (progn (setq prev-walls (nth (1- f) floor-walls-list)) (setq slab-x1 (min curr-min (car (car prev-walls)))) (setq slab-x2 (max curr-max (cadr (last prev-walls))))) 
          (progn (setq slab-x1 curr-min slab-x2 curr-max prev-walls nil))
        )
        (setq y-bottom (+ base-y (nth f heights)) h-next (nth f rel-heights) y-next (+ y-bottom h-next) y-ceil (- y-next slab-thick))
        
        (setvar "CLAYER" dim-layer) 
        (command "_.DIMLINEAR" "_NON" (list slab-x1 y-bottom 0) "_NON" (list slab-x1 y-next 0) "_NON" (list (- slab-x1 50.0) (+ y-bottom (/ h-next 2.0)) 0))
        
        (setq check-walls (if (> f 0) (append prev-walls curr-walls) curr-walls))
        (setq all-voids '())
        (foreach vr void-ranges
          (setq rv-s (if check-walls (sd:snap-to-closest-wall (car vr) check-walls) (car vr)))
          (setq rv-e (if check-walls (sd:snap-to-closest-wall (cadr vr) check-walls) (cadr vr)))
          (if (> (- rv-e rv-s) 1.0) (setq all-voids (cons (list rv-s rv-e) all-voids)))
        )
        (setq ext-min (min slab-x1 curr-min) ext-max (max slab-x2 curr-max))
        
        (setq slab-obs-top '()) (foreach w curr-walls (setq slab-obs-top (cons (list (car w) (cadr w)) slab-obs-top))) (setq slab-obs-top (append slab-obs-top all-voids))
        (setq slab-obs-bot '()) (if (> f 0) (foreach w prev-walls (if (= (caddr w) 'WALL) (setq slab-obs-bot (cons (list (car w) (cadr w)) slab-obs-bot))))) (setq slab-obs-bot (append slab-obs-bot all-voids))
        
        (setvar "CLAYER" slab-layer) 
        (sd:draw-sweep y-bottom slab-layer ext-min ext-max slab-obs-top 0.0)
        (if (> f 0) 
          (progn 
            (sd:draw-sweep (- y-bottom slab-thick) slab-layer ext-min ext-max slab-obs-bot 0.0) 
            (command "_.LINE" "_NON" (list ext-min y-bottom 0) "_NON" (list ext-min (- y-bottom slab-thick) 0) "") 
            (command "_.LINE" "_NON" (list ext-max y-bottom 0) "_NON" (list ext-max (- y-bottom slab-thick) 0) "") 
            (foreach vr all-voids 
              (command "_.LINE" "_NON" (list (car vr) y-bottom 0) "_NON" (list (car vr) (- y-bottom slab-thick) 0) "") 
              (command "_.LINE" "_NON" (list (cadr vr) y-bottom 0) "_NON" (list (cadr vr) (- y-bottom slab-thick) 0) "")
            )
          )
        )
        
        (if draw-plaster 
          (progn 
            (setq exp-curr '()) (foreach w curr-walls (setq exp-curr (cons (list (- (car w) p-thk) (+ (cadr w) p-thk)) exp-curr))) 
            (setq exp-prev '()) (if (> f 0) (foreach w prev-walls (if (= (caddr w) 'WALL) (setq exp-prev (cons (list (- (car w) p-thk) (+ (cadr w) p-thk)) exp-prev))))) 
            (sd:draw-sweep (+ y-bottom p-thk) plaster-layer (- ext-min p-thk) (+ ext-max p-thk) (append exp-curr all-voids) 0.0) 
            (if (> f 0) 
              (progn 
                (sd:draw-sweep (- y-bottom slab-thick p-thk) plaster-layer (- ext-min p-thk) (+ ext-max p-thk) (append exp-prev all-voids) 0.0)
                (setvar "CLAYER" plaster-layer)
                (command "_.LINE" "_NON" (list (- ext-min p-thk) (+ y-bottom p-thk) 0) "_NON" (list (- ext-min p-thk) (- y-bottom slab-thick p-thk) 0) "")
                (command "_.LINE" "_NON" (list (+ ext-max p-thk) (+ y-bottom p-thk) 0) "_NON" (list (+ ext-max p-thk) (- y-bottom slab-thick p-thk) 0) "")
              )
            )
          )
        )
        
        (foreach w curr-walls
          (setq w-x1 (car w) w-x2 (cadr w) w-type (caddr w) is-door nil)
          (foreach d-int door-intervals (if (and (<= (car d-int) (+ w-x2 5.0)) (>= (cadr d-int) (- w-x1 5.0))) (setq is-door T)))
          
          (setq is-void-on-left nil is-void-on-right nil) 
          (foreach vr all-voids (if (<= (abs (- w-x1 (cadr vr))) 1.0) (setq is-void-on-left T)) (if (<= (abs (- w-x2 (car vr))) 1.0) (setq is-void-on-right T)))
          (setq is-outer-left (<= (abs (- w-x1 ext-min)) 5.0) is-outer-right (<= (abs (- w-x2 ext-max)) 5.0) w-y-bot y-bottom w-y-top (if (= w-type 'PARAPET) (+ y-bottom parapet-h) y-ceil))
          
          (setvar "CLAYER" (if is-door opening-layer draft-layer)) 
          (command "_.LINE" "_NON" (list w-x1 w-y-bot 0) "_NON" (list w-x1 w-y-top 0) "") 
          (command "_.LINE" "_NON" (list w-x2 w-y-bot 0) "_NON" (list w-x2 w-y-top 0) "") 
          (if (= w-type 'PARAPET) (command "_.LINE" "_NON" (list w-x1 w-y-top 0) "_NON" (list w-x2 w-y-top 0) ""))
          
          (if draw-plaster 
            (progn 
              (setvar "CLAYER" plaster-layer) 
              (setq p-top-l (if (= w-type 'PARAPET) (+ y-bottom parapet-h p-thk) (- y-ceil p-thk)) p-top-r p-top-l) 
              (setq p-bot-l (cond ((and is-void-on-left (> f 0)) (- y-bottom slab-thick p-thk)) (is-void-on-left y-bottom) ((and is-outer-left (> f 0)) (- y-bottom slab-thick p-thk)) (is-outer-left y-bottom) (T (+ y-bottom p-thk)))) 
              (setq p-bot-r (cond ((and is-void-on-right (> f 0)) (- y-bottom slab-thick p-thk)) (is-void-on-right y-bottom) ((and is-outer-right (> f 0)) (- y-bottom slab-thick p-thk)) (is-outer-right y-bottom) (T (+ y-bottom p-thk)))) 
              
              (command "_.LINE" "_NON" (list (- w-x1 p-thk) p-bot-l 0) "_NON" (list (- w-x1 p-thk) p-top-l 0) "") 
              (command "_.LINE" "_NON" (list (+ w-x2 p-thk) p-bot-r 0) "_NON" (list (+ w-x2 p-thk) p-top-r 0) "") 
              
              (if (= w-type 'PARAPET) 
                (progn 
                  (command "_.LINE" "_NON" (list (- w-x1 p-thk) p-top-l 0) "_NON" (list (+ w-x2 p-thk) p-top-r 0) "")
                ) 
                (if (and draw-plaster (> p-thk 0.0) (or is-outer-left is-outer-right) (equal p-top-l p-top-r 0.5)) 
                  (command "_.LINE" "_NON" (list (- w-x1 p-thk) p-top-l 0) "_NON" (list (+ w-x2 p-thk) p-top-r 0) "")
                )
              )
            )
          )
        )
      )
    )
    (setq f (1+ f))
  )

  ;; --- Phase 3: 屋頂層 ---
  (setq rf-walls (nth (1- h-count) floor-walls-list))
  (setq prev-walls (nth (- h-count 2) floor-walls-list))
  (setq door-intervals (nth (1- h-count) floor-doors-list))
  (if (and rf-walls prev-walls)
    (progn 
      (setq y-bottom (+ base-y (nth (1- h-count) heights)) valid-rf-walls '()) 
      (foreach w rf-walls 
        (setq w-x1 (car w) w-x2 (cadr w)) 
        (if (vl-some (function (lambda (pw) (and (<= (car pw) (+ w-x1 10.0)) (>= (cadr pw) (- w-x2 10.0))))) prev-walls) 
          (setq valid-rf-walls (append valid-rf-walls (list w)))
        )
      ) 
      (if (null valid-rf-walls) (setq valid-rf-walls rf-walls)) 
      (setq rf-min (car (car valid-rf-walls)) rf-max (cadr (last valid-rf-walls))) 
      (setq check-walls (append prev-walls valid-rf-walls)) 
      (setq all-voids '()) 
      (foreach vr void-ranges 
        (setq rv-s (sd:snap-to-closest-wall (car vr) check-walls)) 
        (setq rv-e (sd:snap-to-closest-wall (cadr vr) check-walls)) 
        (if (> (- rv-e rv-s) 1.0) (setq all-voids (cons (list rv-s rv-e) all-voids)))
      ) 
      (setq ext-min (min rf-min (car (car prev-walls))) ext-max (max rf-max (cadr (last prev-walls)))) 
      (setq rf-slab-obs-top '()) (foreach w valid-rf-walls (setq rf-slab-obs-top (cons (list (car w) (cadr w)) rf-slab-obs-top))) (setq rf-slab-obs-top (append rf-slab-obs-top all-voids)) 
      (setq rf-slab-obs-bot '()) (foreach w prev-walls (if (= (caddr w) 'WALL) (setq rf-slab-obs-bot (cons (list (car w) (cadr w)) rf-slab-obs-bot)))) (setq rf-slab-obs-bot (append rf-slab-obs-bot all-voids)) 
      
      (setvar "CLAYER" slab-layer) 
      (sd:draw-sweep y-bottom slab-layer ext-min ext-max rf-slab-obs-top 0.0) 
      (sd:draw-sweep (- y-bottom slab-thick) slab-layer ext-min ext-max rf-slab-obs-bot 0.0) 
      (command "_.LINE" "_NON" (list ext-min y-bottom 0) "_NON" (list ext-min (- y-bottom slab-thick) 0) "") 
      (command "_.LINE" "_NON" (list ext-max y-bottom 0) "_NON" (list ext-max (- y-bottom slab-thick) 0) "") 
      (foreach vr all-voids 
        (command "_.LINE" "_NON" (list (car vr) y-bottom 0) "_NON" (list (car vr) (- y-bottom slab-thick) 0) "") 
        (command "_.LINE" "_NON" (list (cadr vr) y-bottom 0) "_NON" (list (cadr vr) (- y-bottom slab-thick) 0) "")
      ) 
      
      (if draw-plaster 
        (progn 
          (setq exp-curr '()) (foreach w valid-rf-walls (setq exp-curr (cons (list (- (car w) p-thk) (+ (cadr w) p-thk)) exp-curr))) 
          (setq exp-prev '()) (foreach w prev-walls (if (= (caddr w) 'WALL) (setq exp-prev (cons (list (- (car w) p-thk) (+ (cadr w) p-thk)) exp-prev)))) 
          (sd:draw-sweep (+ y-bottom p-thk) plaster-layer (- ext-min p-thk) (+ ext-max p-thk) (append exp-curr all-voids) 0.0) 
          (sd:draw-sweep (- y-bottom slab-thick p-thk) plaster-layer (- ext-min p-thk) (+ ext-max p-thk) (append exp-prev all-voids) 0.0)
          (setvar "CLAYER" plaster-layer)
          (command "_.LINE" "_NON" (list (- ext-min p-thk) (+ y-bottom p-thk) 0) "_NON" (list (- ext-min p-thk) (- y-bottom slab-thick p-thk) 0) "")
          (command "_.LINE" "_NON" (list (+ ext-max p-thk) (+ y-bottom p-thk) 0) "_NON" (list (+ ext-max p-thk) (- y-bottom slab-thick p-thk) 0) "")
        )
      ) 
      
      (foreach w valid-rf-walls 
        (setq w-x1 (car w) w-x2 (cadr w) is-door nil) 
        (foreach d-int door-intervals (if (and (<= (car d-int) (+ w-x2 5.0)) (>= (cadr d-int) (- w-x1 5.0))) (setq is-door T))) 
        (setq is-void-on-left nil is-void-on-right nil) 
        (foreach vr all-voids (if (<= (abs (- w-x1 (cadr vr))) 1.0) (setq is-void-on-left T)) (if (<= (abs (- w-x2 (car vr))) 1.0) (setq is-void-on-right T))) 
        (setq is-outer-left (<= (abs (- w-x1 ext-min)) 5.0) is-outer-right (<= (abs (- w-x2 ext-max)) 5.0) w-y-bot y-bottom) 
        
        (setvar "CLAYER" (if is-door opening-layer draft-layer)) 
        (command "_.LINE" "_NON" (list w-x1 w-y-bot 0) "_NON" (list w-x1 (+ y-bottom parapet-h) 0) "") 
        (command "_.LINE" "_NON" (list w-x2 w-y-bot 0) "_NON" (list w-x2 (+ y-bottom parapet-h) 0) "") 
        (command "_.LINE" "_NON" (list w-x1 (+ y-bottom parapet-h) 0) "_NON" (list w-x2 (+ y-bottom parapet-h) 0) "") 
        
        (if draw-plaster 
          (progn 
            (setvar "CLAYER" plaster-layer) 
            (setq p-bot-l (cond (is-outer-left (- y-bottom slab-thick p-thk)) (is-void-on-left (- y-bottom slab-thick p-thk)) (T (+ y-bottom p-thk)))) 
            (setq p-bot-r (cond (is-outer-right (- y-bottom slab-thick p-thk)) (is-void-on-right (- y-bottom slab-thick p-thk)) (T (+ y-bottom p-thk)))) 
            (command "_.LINE" "_NON" (list (- w-x1 p-thk) p-bot-l 0) "_NON" (list (- w-x1 p-thk) (+ y-bottom parapet-h p-thk) 0) "") 
            (command "_.LINE" "_NON" (list (+ w-x2 p-thk) p-bot-r 0) "_NON" (list (+ w-x2 p-thk) (+ y-bottom parapet-h p-thk) 0) "") 
            (setq top-plaster-x1 (- w-x1 p-thk) top-plaster-x2 (+ w-x2 p-thk)) 
            (command "_.LINE" "_NON" (list top-plaster-x1 (+ y-bottom parapet-h p-thk) 0) "_NON" (list top-plaster-x2 (+ y-bottom parapet-h p-thk) 0) "")
          )
        )
      )
    )
  )

  (vl-catch-all-apply 'setvar (list "CLAYER" old-clayer)) 
  (vla-EndUndoMark doc) 
  (princ "\n=== 繪製完成！ V89 (自適應邊界狙擊雷達版) ===") 
  (princ)
)

(princ "\nSectionDraft V89 載入完成。") 
(princ)