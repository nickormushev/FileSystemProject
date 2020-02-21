module DataTypes where

type Name = String
type Data = String
data FSItem = File { getName :: Name, getData :: Data } | Folder { getName :: Name, getChildren :: [FSItem] } deriving (Eq)
data FSPath = Path Name [FSItem] [FSItem] deriving (Eq)
type FSZipper = (FSItem, [FSPath])

instance Show FSItem where 
    show (Folder name children) = name 
    show (File name d) = name 

instance Show FSPath where 
    show (Path name ls rs) = name

myDisk :: FSItem  
myDisk = 
    Folder "/"   
        [ File "goat_yelling_like_man.wmv" "baaaaaa"  
        , File "pope_time.avi" "god bless"  
        , Folder "pics"  
            [ File "ape_throwing_up.jpg" "bleargh"  
            , File "watermelon_smash.gif" "smash!!"  
            , File "skull_man(scary).bmp" "Yikes!"  
            ]  
        , File "dijon_poupon.doc" "best mustard"  
        , Folder "programs"  
            [ File "fartwizard.exe" "10gotofart"  
            , File "owl_bandit.dmg" "mov eax, h00t"  
            , File "not_a_virus.exe" "really not a virus"  
            , Folder "source_code"  
                [ File "best_hs_prog.hs" "main = print (fix error)"  
                , File "random.hs" "main = print 4"  
                ]  
            ]  
        ]


