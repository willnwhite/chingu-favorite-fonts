module ListExcept exposing (except)


except : List a -> List a -> List a
except aList aList2 =
    List.filter (\a -> not (List.member a aList)) aList2
