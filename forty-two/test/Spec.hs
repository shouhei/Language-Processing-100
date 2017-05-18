{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts, DeriveGeneric, ScopedTypeVariables #-}
import Test.Hspec
import Lib

specMakeTuples :: Spec
specMakeTuples = do
  describe "makeTuple" $ do
    it "can make tuple with thee value list which" $
      makeTuples ['a'] ['b'] ['c'] `shouldBe` [('a', 'b', 'c')]

specDstToInt :: Spec
specDstToInt = do
  describe "dstToInt" $ do
    it "can convert '[1-9]D' to one digit int" $ do
      dstToInt "2D" `shouldBe` 2
    it "can convert '-1D' to -1" $ do
      dstToInt "-1" `shouldBe` -1

specDstToSrc :: Spec
specDstToSrc = do
  describe "dstToStr" $ do
    it "can convert array values to indexes, value indices to values." $ do
      dstToSrc [3,3,5,5,5] `shouldBe` [[],[],[],[0,1],[],[2,3,4]]

specMetaToDst :: Spec
specMetaToDst = do
  describe "metaToDst" $ do
    it "can pull out dst number from cabocha's meta string" $ do
      metaToDst "* 1 2D" `shouldBe` 2

specMakeMorphList :: Spec
specMakeMorphList = do
  describe "makeMorphList" $ do
    it "can make morph list form cabocha's analyzed strings" $ do
      makeMorphList ["見当\t名詞,サ変接続,*,*,*,*,見当,ケントウ,ケントー","が\t助詞,格助詞,一般,*,*,*,が,ガ,ガ"] `shouldBe` [Morph{surface="見当", base="見当", pos="名詞", pos1="サ変接続"}, Morph{surface="が", base="が", pos="助詞", pos1="格助詞"}]

specMakeChunk :: Spec
specMakeChunk = do
  describe "makeChunk" $ do
    it "can make chunk from morphs, dst, src" $ do
      makeChunk ["見当\t名詞,サ変接続,*,*,*,*,見当,ケントウ,ケントー"] 1 [2,3] `shouldBe` Chunk{morphs=[Morph{surface="見当", base="見当", pos="名詞", pos1="サ変接続"}], dst=1, srcs=[2,3]}

specMakeChunkWithDstStr :: Spec
specMakeChunkWithDstStr = do
  describe "makeChunkWithDstStr" $ do
    it "can make tsv which has a word and dst words from Chunks" $ do
      makeChunkWithDstStr [Chunk{morphs=[Morph{surface="見当", base="見当", pos="名詞", pos1="サ変接続"}], dst=1, srcs=[2,3]},Chunk{morphs=[Morph{surface="見当", base="見当", pos="名詞", pos1="サ変接続"}], dst=(-1), srcs=[2,3]}]`shouldBe` ["見当\t見当"]

main :: IO ()
main = hspec $ do
  specMakeTuples
  specDstToInt
  specDstToSrc
  specMetaToDst
  specMakeMorphList
  specMakeChunk
  specMakeChunkWithDstStr
