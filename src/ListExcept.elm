module ListExcept exposing (except)

-- > ListExcept.except [1,2] [1,2,3,4]
-- [3,4] : List number


except : List a -> List a -> List a
except aList aList2 =
    List.filter (\a -> not (List.member a aList)) aList2
