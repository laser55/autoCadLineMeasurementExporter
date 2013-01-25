(defun c:dwrite(/ plObj stBl enBl datLst cAns hyObj okFlg cMat mLst mNum)

; ************ MODIFY LIST OF MATERIALS ************

  (setq mLst '(
	       (0 . "N/A")
	       (1 . "DIN 2391")
	       (2 . "DIN 2392")
	       (3 . "DIN 2394")
	       )
	); end setq
  

; ************ MODIFY DEFAULT MATERIAL ************

  (if(not dwrite:material)
     (setq dwrite:material "N/A")
    ); end if
  
  
  
  (defun Entsel_or_Text(Spaces Message / lChr tStr grLst filPt
			selSet outVal pSps)
  (princ Message)
  (setq tStr ""); end setq
  (if Spaces
    (setq pSps(list "\r"))
    (setq pSps(list " " "\r"))
    ); end if
   (while
      (and
        (not(member lChr pSps))
	(/= 3(car grLst))
        ); end and
      (if
        (setq grLst(grread nil 4 2))
        (progn
         (cond
          ((= 3(car grLst))
           (setq filPt(cadr grLst)
                 selSet(ssget filPt)
                 ); end setq
           (if selSet
                (setq outVal
                (list(ssname selSet 0)filPt))
             ); end if
           ); end condition #1
	  ((or
	     (equal '(2 13) grLst)
	     (equal 25(car grLst))
	     ); end or
	    (setq lChr "\r"
		  outVal tStr); end setq
	   ); end condition #2
	  ((and
	     (equal '(2 8) grLst)
	     (< 0(strlen tStr))
	     ); end and
	   (setq tStr(substr tStr 1(1-(strlen tStr))))
	   (princ(strcat(chr 8)(chr 32)(chr 8)))
	   ); end condition #3
          ((and
	     (= 2(car grLst))
	     (<= 32(cadr grLst)126)
	     ); end and
           (setq lChr(chr(cadr grLst)))
           (if(not(member lChr pSps))
                 (progn
                 (setq tStr(strcat tStr lChr)
                       outVal tStr); end setq
             (princ lChr)
           ); end progn
          ); end if
         ); end condition #4
        ); end cond
       ); end progn
      ); end if
     ); end while
    outVal
 ); end of Entsel_or_Text
  

  (while(not okFlg)
    (princ(strcat "\nCurrent material = " dwrite:material))
    
    (setq plObj(entsel "\nSelect polyline or line: "))

    (cond
      ((and
	  (= 'LIST(type plObj))
	  ;(= "LWPOLYLINE"(cdr(assoc 0(entget(car plObj)))))
	 ); end and
       (setq plObj(car plObj)
	     okFlg T); end setq
       ); end condition #1
      ((= 'LIST(type plObj))
       (princ "\nThis isn't LwPolyline! ")
       ); end condition #2
      ((and
	 (= 'STR(type plObj))
	 (member(strcase plObj) '("M" "_M" "MATERIAL" "_MATERIAL"))
	 
	 ); end and
       (textscr)
       (princ "\n====== MATERIAL LIST ======")
       (foreach m mLst
	 (princ(strcat "\n[" (itoa(car m)) "] - "(cdr m)))
	 ); end foreach
       (princ "\n===========================")
       (setq mNum(getint "\nSelect material from list: "))
       (if(and mNum(setq cMat(assoc mNum mLst)))
	 (progn
	   (setq dwrite:material(cdr cMat))
	   (graphscr)
	   ); end progn
	 (princ "\nCan't find material with this number! ")
	 ); end if
       ); end condition #3
      ((null plObj)
        (princ "\nEmpty selection! ")
       ); end condition #4
      (T
       (princ "\nInvalid keyword option! ")
       ); end condition #5
      ); end cond
    ); end while
  (while(not stBl)
    (prompt "\n")
    ;(prompt ("exeis dwsei ta")
    (princ (cdr shmeia))
    (setq stBl(Entsel_or_Text T "\ndwse ta shmeia: "))
    (setq test (list (print stBl)))
    (setq shmeia (append  shmeia test))
    (cond
      ((and
	 (= 'LIST(type stBl))
	 (= "INSERT"(cdr(assoc 0(entget(car stBl)))))
	); end and
        (setq stBl(cdr(assoc 2(entget(car stBl)))))
       ); end condition #1
      ((= 'LIST(type stBl))
        (princ "\nThis isn't block! ")
        (setq stBl nil)
       ); end condition #2
      ((null stBl)
       (princ "\nEmpty input! ")
       ); end condition #3
      ); end cond
    ); end while
; (while(not enBl)
 ;   (setq enBl(Entsel_or_Text T "\nSelect 'End' block or type name: "))
  ;  (cond
   ;   ((and
;	 (= 'LIST(type enBl))
;	 (= "INSERT"(cdr(assoc 0(entget(car enBl)))))
;	); end and
 ;       (setq enBl(cdr(assoc 2(entget(car enBl)))))
  ;     ); end condition #1
   ;   ((= 'LIST(type enBl))
    ;    (princ "\nThis isn't block! ")
     ;   (setq enBl nil)
      ; ); end condition #2
      ;((null enBl)
       ;(princ "\nEmpty input! ")
       ;); end condition #3
      ;); end cond
    ;); end while
  (setq datLst(list(cons 1 stBl)(cons 2 enBl))
	plObj(vlax-ename->vla-object plObj)
	hyObj(vla-get-Hyperlinks plObj)
	); end setq
  (if(vlax-ldata-get plObj "PipeData")
      (progn
	(initget "Yes No")
	(setq cAns(getkword "\nPipe data already exists. Overwrite? [Yes/No]<Yes>: "))
	(if(null cAns)(setq cAns "Yes"))
	(if(= "Yes" cAns)
	  (progn
	   (vlax-ldata-delete plObj "PipeData")
	   (vlax-ldata-put plObj "PipeData" datLst)
	   (vla-Delete(vla-Item hyObj 0))
	   (vla-Add hyObj "Has Pipe Data" (strcat "ID: "  (itoa(vla-get-ObjectID plObj))
					          "\nMaterial: " dwrite:material
					          "\nLength: " (rtos(vlax-curve-GetDistAtParam plObj
						                       (vlax-curve-GetEndParam plObj)))))
	   (princ "\n<<< Data successfuly added >>> ")
	   ); end progn
	  ); end if
	); end progn
      (progn
       (vlax-ldata-put plObj "PipeData" datLst)
       (vla-Add hyObj "Has Pipe Data" (strcat "ID: "  (itoa(vla-get-ObjectID plObj))
					      "\nMaterial: " dwrite:material
					      "\nLength: " (rtos(vlax-curve-GetDistAtParam plObj
						                   (vlax-curve-GetEndParam plObj)))))
       (princ "\n<<< Data successfuly added >>> ")
       ); end progn
      ); end if	
  (princ)



  


(progn
	  ;(setq fDescr(open
	;		(setq fName(strcat(vl-filename-directory(getvar "SAVENAME"))
	;		  "\\"(vl-filename-base(getvar "DWGNAME")) ".csv")) "w"))
	  
	  
	          (write-line(strcat ;(itoa(nth 0 itm))
				  (strcat;(rtos(* 1000.0(nth 0 itm)))
				 (vla-get-Layer plObj)
				 ";" stBl
				; ";" (nth 2 itm)
				 ;";" (nth 2 itm)

				 ;(vl-string-subst "We" "I" S)
				 ";" (vl-string-subst "," "." (rtos(vlax-curve-GetDistAtParam plObj
						                   (vlax-curve-GetEndParam plObj))))
				 )
			  ); end strcat
		fDescr)
	   
	  ;(close fDescr)
  )



  
 ); c:dwrite




(defun c:dcollect(/ plSet oLst cDat fDescr fName cAns exApp wbCol cDoc)
  (if(setq plSet(ssget "_X"))
    (progn
      (foreach pl (mapcar 'vlax-ename->vla-object
                     (vl-remove-if 'listp
                       (mapcar 'cadr(ssnamex plSet))))
	(if(setq cDat(vlax-ldata-get pl "PipeData"))
	  (progn
	    (setq oLst(cons
			(list
			     ;(vla-get-ObjectID pl)
			     ;(vla-get-ConstantWidth pl)
			     (vla-get-Layer pl)
			     (cdr(assoc 1 cDat))
			     ;(cdr(assoc 2 cDat))
			     ;(cdr(assoc 3 cDat))
			     (vlax-curve-GetDistAtParam pl
			       (vlax-curve-GetEndParam pl))
			    ); end list
		      oLst); end cons
		  ); end setq
	    ); end progn
	  ); end if
	); end foreach
      (if oLst
	(progn
	  (setq fDescr(open
			(setq fName(strcat(vl-filename-directory(getvar "SAVENAME"))
			  "\\"(vl-filename-base(getvar "DWGNAME")) ".csv")) "w"))
	  (write-line "Layer;Shmeia;Length" fDescr)
	  (foreach itm oLst
	          (write-line(strcat ;(itoa(nth 0 itm))
				  (strcat;(rtos(* 1000.0(nth 0 itm)))
				 (nth 0 itm)
				 ";" (nth 1 itm)
				; ";" (nth 2 itm)
				 ;";" (nth 2 itm)

				 ;(vl-string-subst "We" "I" S)
				 ";" (vl-string-subst "," "." (rtos(nth 2 itm)))        )
			  ); end strcat
		fDescr)
	    ); end foreach
	  (close fDescr)
	  (princ(strcat "\nCSV file location: " fName ))
	  (initget "Yes No")
	  (setq cAns(getkword "\nOpen file [Yes/No]: "))
	  (if(= cAns "Yes")
	    (if(setq exApp(vlax-get-or-create-object "Excel.Application"))
	      (progn
		(vlax-put-property exApp 'Visible :vlax-true)
	        (setq wbCol(vlax-get-property exApp 'Workbooks)
		      cDoc(vlax-invoke-method wbCol 'Open fName))
		(vlax-release-object cDoc)
		(vlax-release-object wbCol)
		(vlax-release-object exApp)
		); end progn
	      ); end if
	    ); end if
	  ); end progn
	(princ "\nNo data found! ")
	); end if
      ); end progn
  ); end if
  (princ)
  ); end of c:dcollect



(defun c:ddata(/ cEnt cDat cPln)
  (if(setq cEnt(entsel "\nSelect polyline to view data: "))
    (if(= "LWPOLYLINE"(cdr(assoc 0(entget(car cEnt)))))
      (if(setq cDat(vlax-ldata-get(setq cPln(vlax-ename->vla-object(car cEnt))) "PipeData"))
	(alert(strcat
		"                   PIPE DATA                 \n"
		"\nPipe ID: " (itoa(vla-get-ObjectID cPln))
		"\nDiameter: " (rtos(* 1000.0(vla-get-ConstantWidth cPln)2 0))
		"\nLayer: " (vla-get-Layer cPln)
		"\nFrom: " (cdr(assoc 1 cDat))
		"\nTo: " (cdr(assoc 2 cDat))
		"\nMaterial: " (cdr(assoc 3 cDat))
		"\nLength: " (rtos(vlax-curve-GetDistAtParam cPln
			           (vlax-curve-GetEndParam cPln)))
		); end strcat
	      ); end alert
	(princ "\nNo data found! ")
	); end if
      (princ "\nThis isn't LwPolyline! ")
      ); end if
    (princ "\nNothing selected! ")
    ); end if
  (princ)
  ); end of c:ddata



(defun c:ddelete(/ cCnt cAns plSet cDat hyCol cHyp)
  (initget 1 "All Selection")
  (setq cAns(getkword "\nWhich data to delete [All/Selection]: ")
	cCnt 0)
  (if(= cAns "All")
    (setq plSet(ssget "_X"))
    ); end if
  (getstring "\n*** WARNING! All data will deleted. Enter to Continue or Esc to Quite. ***")
  (if plSet
    (progn
      (foreach pl (mapcar 'vlax-ename->vla-object
                     (vl-remove-if 'listp
                       (mapcar 'cadr(ssnamex plSet))))
	(if(setq cDat(vlax-ldata-get pl "PipeData"))
	  (progn
	    (vlax-ldata-delete pl "PipeData")
	    (setq cCnt(1+ cCnt))
	    ); end progn
	  ); end if
            (vlax-for hy(vla-get-Hyperlinks pl)
	      (if(= "Has Pipe Data"(vla-get-URL hy))
		(vla-Delete hy)
		); end vlax-for
	      ); end vlax-for
	); end foreach
      (if(/= 0 cCnt)
	(princ(strcat "\n<<< " (itoa cCnt) " item(s) was deleted >>> "))
	(princ "\nNothing data found! ")
	); end if
      ); end progn
    ); end if
  (princ)
  ); end of c:ddelete


(defun c:dniki()


(setq shmeia (list "aaaa" "exeis epile3ei hdh ta:"))
  (progn
	  (setq fDescr(open
			(setq fName(strcat(vl-filename-directory(getvar "SAVENAME"))
			  "\\"(vl-filename-base(getvar "DWGNAME")) ".csv")) "w"))
	  (write-line "Layer;Shmeia;Length" fDescr)
    ); end progn



  
  (while(> 1 0)
   
   (progn
     (c:dwrite)
     (setq ab "asda")
     
    )
    );end while
  )


(defun c:dopen()
  (progn
  (setq fName(strcat(vl-filename-directory(getvar "SAVENAME"))
			  "\\"(vl-filename-base(getvar "DWGNAME")) ".csv"))
(setq exApp(vlax-get-or-create-object "Excel.Application"))
	      (progn
		(vlax-put-property exApp 'Visible :vlax-true)
	        (setq wbCol(vlax-get-property exApp 'Workbooks)
		      cDoc(vlax-invoke-method wbCol 'Open fName))
		(vlax-release-object cDoc)
		(vlax-release-object wbCol)
		(vlax-release-object exApp)
		); end progn
  )
  )



(vl-load-com) 
  
  