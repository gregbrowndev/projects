import           Control.Applicative
import           Control.Monad
{- Lesson 33: Capstone: SQL-Like Queries in Haskell

This capstone covers:

    - Using the Monad type class to create SQL-like queries on lists
    - Generalising function written for on Monad (for example, List) to many
    - Organising functions with types

In the last lesson, we saw how List can be understood as a Monad enabling list
comprehension. In this capstone, we'll take this even further by creating a SQL-
like interface to lists (and other Monads) that we'll call HINQ (similar to the
.Net tool LINQ). In the end, we'll have a tool that:

    - Provides a familiar interface for querying relational data in Haskell
    - Is strongly typed
    - Uses lazy evaluation to allow you to pass around queries without executing
      them
    - Can be used seamlessly with other Haskell functions
-}

{- 33.1 Getting started

We'll be working with a data model involving students, teachers, courses, and
enrollments.
-}
data Name = Name {
    firstName :: String,
    lastName  :: String
}
instance Show Name where
    show (Name first last) = mconcat [first, " ", last]

data GradeLevel = Freshman | Sophmore | Junior | Senior
  deriving (Eq, Ord, Enum, Show)

data Student = Student {
    studentId   :: Int,
    gradeLevel  :: GradeLevel,
    studentName :: Name
} deriving Show

students :: [Student]
students = [(Student 1 Senior (Name "Audre" "Lorde"))
            ,(Student 2 Junior (Name "Leslie" "Silko"))
            ,(Student 3 Freshman (Name "Judith" "Butler"))
            ,(Student 4 Senior (Name "Guy" "Debord"))
            ,(Student 5 Sophmore (Name "Jean" "Baudrillard"))
            ,(Student 6 Junior (Name "Julia" "Kristeva"))]

{- 33.2 Basic select and where queries

Note: all functions will be prefixed with _ since we haven't covered modules yet,
and we need to avoid name collisions (where is a keyword).

Note: these functions include the monadic type constraints from section 33.5.
-}
--_select :: (a -> b) -> [a] -> [b]
_select :: Monad m => (a -> b) -> m a -> m b
_select prop vals = do
    val <- vals
    return (prop val)

{-
    ghci> _select (firstName . studentName) students
    ["Audre","Leslie","Judith","Guy","Jean","Julia"]

    ghci> _select gradeLevel students
    [Senior,Junior,Freshman,Senior,Sophmore,Junior]

    ghci> _select (\x -> (studentName x, gradeLevel x)) students
    [(Audre Lorde,Senior),
     (Leslie Silko,Junior),
     (Judith Butler,Freshman),
     (Guy Debord,Senior),
     (Jean Baudrillard,Sophmore),
     (Julia Kristeva,Junior)]

Note: our _select function is much less powerful than fmap since fmap works on
any Functor. Later in the capstone we'll refactor this.
-}

--_where :: (a -> Bool) -> [a] -> [a]
_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where test vals = do
    val <- vals
    guard (test val)
    return val

-- test predicate
startsWith :: Char -> String -> Bool
startsWith char string = char == (head string)

{-
    ghci> _where (startsWith 'J' . firstName) (_select studentName students)
    [Judith Butler,Jean Baudrillard,Julia Kristeva]
-}

{- 33.3 Joining Course and Teacher data types -}
data Teacher = Teacher {
    teacherId   :: Int,
    teacherName :: Name
} deriving Show

teachers :: [Teacher]
teachers = [Teacher 100 (Name "Simone" "De Beauvior")
 ,Teacher 200 (Name "Susan" "Sontag")]

data Course = Course {
    courseId    :: Int,
    courseTitle :: String,
    teacher     :: Int
} deriving Show

courses :: [Course]
courses = [Course 101 "French" 100
  ,Course 201 "English" 200]

-- Implement join by computing the cartesian product of the two list, and then
-- filter all possible pairs.
--_join :: [a] -> [b] -> (a -> c) -> (b -> c) -> [(a,b)]
_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a,b)
_join data1 data2 prop1 prop2 = do
 d1 <- data1
 d2 <- data2
 let dpairs = (d1,d2)
 guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
 return dpairs

-- Remember that in do-notation, the let dpairs = (d1, d2) represents the
-- combination of all possible pairs
{-
    ghci> _join teachers courses teacherId teacher
    [(Teacher {teacherId = 100, teacherName = Simone De Beauvior},
      Course {courseId = 101, courseTitle = "French", teacher = 100}),
     (Teacher {teacherId = 200, teacherName = Susan Sontag},
      Course {courseId = 201, courseTitle = "English", teacher = 200})
    ]
-}

{- 33.4 Building your HINQ interface and example queries

With _select, _where, and _join we can start packaging our logic into an
easier format. E.g.

    joinData = (_join teachers courses teacherId teacher)
    whereResult = _where ((== "English") . courseTitle . snd) joinData
    selectResult = _select (teacherName . fst) whereResult

This solution is OK but not very SQL-like. Instead, we'll make a function
_hinq that will take three lambda functions in the expected order.
-}

_hinq selectQuery joinQuery whereQuery = (\joinData ->
    (\whereResult ->
        selectQuery whereResult)(whereQuery joinData)
    ) joinQuery

finalResult :: [Name]
finalResult = _hinq
    (_select (teacherName . fst))
    (_join teachers courses teacherId teacher)
    (_where ((== "English") .courseTitle . snd))

{-
    ghci> finalResult
    [Susan Sontag]

This isn't a perfect representation of SQL or LINQ, but it's good enough.
However, one annoyance we can improve is the fact that we must always provide
a where clause. There are no default args in Haskell, so we would need to write
something like:

    teacherFirstName :: [String]
    teacherFirstName = _hinq (_select firstName)
     finalResult
     (_where (\_ -> True))

We can improve this by introducing a HINQ type.
-}

{- 33.5 Making a HINQ type for your queries

In order to make the where clause optional, we'll introduce a new type HINQ.
However, first we need to tweak our functions so they work with any Monad.
In order to generalise our functions, we just need to change the type signatures.
If we had used list-specific functions, like map and filter, we would have had to
refactor the logic as well.
-}

data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a)
                | HINQ_ (m a -> m b) (m a)

-- The second parameter allows for a _join expression or plain data.
-- We can write a wrapper function to make this type easier to use.

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (\_ -> True))

-- lets test it
query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ (_select (teacherName . fst))
              (_join teachers courses teacherId teacher)
              (_where ((== "English") .courseTitle . snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName)
               teachers

{-
    ghci> runHINQ query1
    [Susan Sontag]
    ghci> runHINQ query2
    [Simone De Beauvior,Susan Sontag]
-}

{- 33.6.1 Using HINQ with Maybe types

Since we refactored our functions to work with any Monad and Alternative type,
let's look at an example of querying a Maybe.
-}
possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

missingCourse :: Maybe Course
missingCourse = Nothing

maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher possibleCourse teacherId teacher)
                   (_where ((== "French") . courseTitle . snd))

maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher missingCourse teacherId teacher)
                   (_where ((== "French") . courseTitle . snd))

{-
    ghci> runHINQ maybeQuery1
    Just Simone De Beauvior

    ghci> runHINQ maybeQuery2
    Nothing
-}

{- 33.6.2 Joining multiple lists to get all enrollments

Now we'll look at querying data to determine course enrollment. We want to get
a list of all the student's names paired with the name of the course they're
enrolled in. To do this we need to join students, enrollments, and courses.
-}

data Enrollment = Enrollment {
    student :: Int,
    course  :: Int
} deriving Show

enrollments :: [Enrollment]
enrollments = [(Enrollment 1 101)
 ,(Enrollment 2 101)
 ,(Enrollment 2 201)
 ,(Enrollment 3 101)
 ,(Enrollment 4 201)
 ,(Enrollment 4 101)
 ,(Enrollment 5 101)
 ,(Enrollment 6 201) ]

studentEnrollmentsQ :: HINQ [] (Student, Enrollment) (Name, Int)
studentEnrollmentsQ = HINQ_ (_select (\(st, en) ->
                                    (studentName st, course en)
                             ))
                             (_join students enrollments studentId student)

studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQ

{-
    ghci> studentEnrollments
    [(Audre Lorde,101),
     (Leslie Silko,101),
     (Leslie Silko,201),
     (Judith Butler,101),
     (Guy Debord,201),
     (Guy Debord,101),
     (Jean Baudrillard,101),
     (Julia Kristeva,201)]

Let's filter this to all English students
-}
englishStudentsQ :: HINQ [] ((Name, Int), Course) Name
englishStudentsQ = HINQ (_select (fst . fst))
                        (_join studentEnrollments courses snd courseId)
                        (_where ((== "English") . courseTitle . snd))

englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ

{-
    ghci> englishStudents
    [Leslie Silko,Guy Debord,Julia Kristeva]

We can use HINQ inside another function to make generic tools for querying:
-}
getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
  where
    courseQuery = HINQ (_select (fst . fst))
                       (_join studentEnrollments courses snd courseId)
                       (_where ((== courseName) . courseTitle . snd))

{-
    ghci> getEnrollments "English"
    [Leslie Silko,Guy Debord,Julia Kristeva]

    ghci> getEnrollments "French"
    [Audre Lorde,Leslie Silko,Judith Butler,Guy Debord,Jean Baudrillard]
-}

{- Summary

By combining monads, sum types (your HINQ type), lazy evaluation, and first-class
functions, you were able to build a powerful query engine from scratch!

In this capstone I:

    - learned how to easily implement _select and _where for lists
    - Used the Cartesian product of two lists to replicate a database join
    - Easily changed functions on lists to functions on monads in general
    - Saw how lambda functions can allow you to restructure the way functions are
      called
    - Made working with HINQ queries easier by using a HINQ data type
-}

{- Extension

Try to implement Semigroup and Monoid for the HINQ type. With both Semigroup and
Monoid, we'll be able to combine multiple HINQ queries into a single query.
-}
--instance Semigroup HINQ m a b
--  where
--    (<>) query1 query2 = HINQ m
