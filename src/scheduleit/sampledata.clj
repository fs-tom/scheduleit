(ns scheduleit.sampledata)

(defn sample-data [& {:keys [small?] :or {small? true}}]
  (let [
        ;;     This is just some lame stuff.
        ;;     #total number of MTT teams
        MTT (if small? 1 10)
        ;; #total weeks
        WKS (if small? 52 156)

        years (quot WKS 52)
        ;;     #mtt teams
        ms  (range MTT)
        ;; weeks
        ws (range WKS)
        ;;     #unit types
        types #{"cst" "dcrf" "cre" "sr" "hrf" "cerfp"}
        ;;     #mapping of unit->type
        ;;     this is one cough-syrup addled way to do it
        ;;     with dictionary comprehensions.
        csts   (indexed-value   0  55 "cst")
        dcrfs  (indexed-value   55 61  "dcrf")
        cres   (indexed-value   61 65 "cre")
        srs    (indexed-value   65 77 "sr")
        hrfs   (indexed-value   77 87  "hrf")
        cerfps (indexed-value   87 104 "cerfp")
        ;;     #assuming we have a table of id->type
        unit-type (merge csts,dcrfs,cres,srs,hrfs,cerfps)
        ;;     #lame total unit count
        total-units (if small? 1 #_10 (count unit-type))
        ;;     #range of unit ids...
        us (range total-units)
        ;;     # training interval relative to type, weeks
        interval-type   {"sr"    13
                         "cre"   52
                         "dcrf"  52
                         "cst"   78
                         "hrf"   78
                         "cerfp" 78}
        ys   (range years)
        constant-demands  {"sr" 6 "dcrf" 6  "hrf" 10 "cerfp" 17}
        static-demands (->> (for [[t y] (product types ys)
                                  :when (constant-demands t)]
                              [[t y] (constant-demands t) ])
                            (into {}))

        variable-demands  {["cre" 1] 1
                           ["cre" 2] 4
                           ["cre" 3] 2}

        ;;     #all the yearly demands.
        ;;     #yearly demands can be trivially translated to weekly
        ;;     #demands...
        yearly-demands (merge variable-demands static-demands)]
    {:MTT MTT,
     :WKS WKS,
     :ms ms,
     :ws ws,
     :types types,
     :total-units total-units,
     :us  (range total-units),
     :unit-type unit-type,
     :interval-type interval-type,
     :msn-demand yearly-demands}))
