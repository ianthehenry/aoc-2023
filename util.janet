(defn ^ [chars] ~(* (not (set ,chars)) 1))
