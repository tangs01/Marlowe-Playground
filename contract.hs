{-# LANGUAGE OverloadedStrings #-}
module Example where

import           Language.Marlowe

main :: IO ()
main = print . pretty $ contract 


{- Define a contract, Close is the simplest contract which just ends the contract straight away
-}
payment :: Contract
payment = (Pay
                                    (Role "escrow")
                                    (Account (Role "seller"))
                                    (Token "" "")
                                    (Constant 500)
                                    Close 
                                )         

refund :: Contract
refund = (Pay
                (Role "escrow")
                (Account (Role "buyer"))
                (Token "" "")
                (Constant 500)
                Close 
            )                                   

contract :: Contract 

contract = When
    [Case
        (Deposit
            (Role "escrow")
            (Role "buyer")
            (Token "" "")
            (Constant 500)
        )
        (When
            [Case
                (Choice
                    (ChoiceId
                        "jobdone"
                        (Role "seller")
                    )
                    [Bound 0 1]
                )
                (If
                    (ValueEQ
                        (ChoiceValue
                            (ChoiceId
                                "jobdone"
                                (Role "seller")
                            ))
                        (Constant 1)
                    )
                    (When
                        [Case
                            (Choice
                                (ChoiceId
                                    "jobdone"
                                    (Role "buyer")
                                )
                                [Bound 0 1]
                            )
                            (If
                                (ValueEQ
                                    (ChoiceValue
                                        (ChoiceId
                                            "jobdone"
                                            (Role "buyer")
                                        ))
                                    (ChoiceValue
                                        (ChoiceId
                                            "jobdone"
                                            (Role "seller")
                                        ))
                                )
                                payment
                                (When
                                    [Case
                                        (Choice
                                            (ChoiceId
                                                "jobdone"
                                                (Role "escrow")
                                            )
                                            [Bound 0 1]
                                        )
                                        (If
                                            (ValueEQ
                                                (ChoiceValue
                                                    (ChoiceId
                                                        "jobdone"
                                                        (Role "escrow")
                                                    ))
                                                (ChoiceValue
                                                    (ChoiceId
                                                        "jobdone"
                                                        (Role "seller")
                                                    ))
                                            )
                                            payment
                                            refund
                                        )]
                                    45
                                    refund
                                )
                            )]
                        25
                        payment
                    )
                    refund
                )]
            15
            refund
        )]
    5 Close 