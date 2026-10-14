import outlines
from typing import List, Optional
from pydantic import BaseModel
model = outlines.models.transformers("microsoft/Phi-3-mini-4k-instruct")

class IntentModel(BaseModel):
    time: Optional[str]
    counterparty: Optional[str]
    action: str
    sell: List[str]
    buy: List[str]

@outlines.prompt
def take_message(message):
    """You are a cryptocurrency trading assistant. Users \
    send you messages from which you need to extract:

    1. the intended action
    2. any potential counterparty information or restrictions for the trade
    3. any potential time and date restrictions
    4. the assets that the the user would be sending, receiving or both.

    The assets can be specified with exact values or bounds for the amounts, ahd using one
    of the following currencies: USDT, USDC, BTC, ETH.

    # EXAMPLE

    MESSAGE: I would like to buy 1 BTC for 90k USDT by midnight today
    RESULT: {"action": "buy", "counterparty": "any", "time": "by midnight today", "buy": ["1 BTC"], "sell": ["90k USDT"] }

    # OUTPUT INSTRUCTIONS

    Answer in valid JSON. Here are the different objects relevant for the output:

    IntentModel:
        action (str): intended action by the user
        counterparty (Optional[str]): counterparty information
        time (Optional[str]): time information/restrictions
        buy (List[str]): assets that would move out of the user's posession in the trade
        sell (List[str]): assets that the user would acquire in the trade

    Return a valid JSON of type "IntentModel"

    # OUTPUT

    ORDER: {{ message }}
    RESULT: """

generator = outlines.generate.json(model, IntentModel)

while True:
    data = input("> Enter trade description\n")
    if data == "q":
        break
    r = generator([take_message(data)])
    print(r)
