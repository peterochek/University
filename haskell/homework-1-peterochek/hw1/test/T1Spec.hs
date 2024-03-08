module T1Spec where

import Test.Hspec

import HW1.T1

spec :: Spec
spec = do
  describe "nextDay" $
    it "returns the tomorrow day for a given one" $ do
      nextDay Sunday    `shouldBe` Monday
      nextDay Monday    `shouldBe` Tuesday
      nextDay Tuesday   `shouldBe` Wednesday
      nextDay Wednesday `shouldBe` Thursday
      nextDay Thursday  `shouldBe` Friday
      nextDay Friday    `shouldBe` Saturday
      nextDay Saturday  `shouldBe` Sunday

  describe "afterDays" $
    it "returns the day in the given number of days from taken" $ do
      afterDays 0 Monday    `shouldBe` Monday
      afterDays 1 Monday    `shouldBe` Tuesday
      afterDays 2 Monday    `shouldBe` Wednesday
      afterDays 3 Monday    `shouldBe` Thursday
      afterDays 4 Monday    `shouldBe` Friday
      afterDays 5 Monday    `shouldBe` Saturday
      afterDays 6 Monday    `shouldBe` Sunday
      afterDays 7 Monday    `shouldBe` Monday

      afterDays 0 Friday    `shouldBe` Friday
      afterDays 1 Friday    `shouldBe` Saturday
      afterDays 2 Friday    `shouldBe` Sunday
      afterDays 3 Friday    `shouldBe` Monday
      afterDays 4 Friday    `shouldBe` Tuesday
      afterDays 5 Friday    `shouldBe` Wednesday
      afterDays 6 Friday    `shouldBe` Thursday
      afterDays 7 Friday    `shouldBe` Friday

      afterDays 48 Saturday   `shouldBe` Friday

  describe "isWeekend" $
    it "shows whether the day is weekend or not" $ do
      isWeekend Sunday    `shouldBe` True
      isWeekend Monday    `shouldBe` False
      isWeekend Tuesday   `shouldBe` False
      isWeekend Wednesday `shouldBe` False
      isWeekend Thursday  `shouldBe` False
      isWeekend Friday    `shouldBe` False
      isWeekend Saturday  `shouldBe` True

  describe "daysToParty" $
    it "shows amount of days left until friday" $ do
      daysToParty Sunday    `shouldBe` 5
      daysToParty Monday    `shouldBe` 4
      daysToParty Tuesday   `shouldBe` 3
      daysToParty Wednesday `shouldBe` 2
      daysToParty Thursday  `shouldBe` 1
      daysToParty Friday    `shouldBe` 0
      daysToParty Saturday  `shouldBe` 6
