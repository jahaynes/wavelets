import qualified Data.Smashy.Map                 as HM
import Types

main :: IO ()
main = do

    hm <- loadTermMap
    rm <- createReverseMap

    rm' <- HM.terms rm (\rmi term -> do
               Just (TermInfo _ tid _ _) <- HM.get hm term
               HM.storeOne rmi (tid, term)) hm

    HM.close rm'
    HM.close hm