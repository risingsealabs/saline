# https://llama-cpp-python.readthedocs.io/en/latest/
# https://python.useinstructor.com/integrations/llama-cpp-python/#llama-cpp-python
from llama_cpp import Llama
from llama_cpp.llama_speculative import LlamaPromptLookupDecoding
import instructor
from enum import Enum
from typing import List, Optional, Annotated, Literal
from annotated_types import Gt, Le

from pydantic import BaseModel

# llm = Llama.from_pretrained(
#     repo_id="bartowski/Llama-3.2-3B-Instruct-GGUF",
#     filename="*Q8_0.gguf",
#     verbose=False
# )
llm = Llama(
    model_path="./Llama-3.2-3B-Instruct-Q8_0.gguf",
    chat_format="chatml",
    n_ctx=2000,
    draft_model=LlamaPromptLookupDecoding(num_pred_tokens=2),
    logits_all=True,
    verbose=True
)

create = instructor.patch(
    create=llm.create_chat_completion_openai_v1,
    mode=instructor.Mode.JSON_SCHEMA,
)

class TokenModel(BaseModel):
    ticker: Literal['BTC', 'ETH', 'USDC', 'USDT', 'SALT']

class TransferModel(BaseModel):
  tag: str = "TransferFunds"
  source: str = "nacl:0xa947ddcc9264a722671c6e4e283cf0e0f3d9cd7baadf5a67e5bbb81865f2560eb80e94591bdc4a80027f2c728be3a7cd"
  target: str
  funds: list[tuple[Literal['BTC', 'ETH', 'USDC', 'USDT', 'SALT'],Annotated[int, Gt(0), Le(100)]]]

prompt = "You are an assistant that should understand the trade described by the user, trying to extract timing information, counterparty information, and what currencies and amounts the user is willing to give up and acquire. Supported currencies are USDT, USDC, ETH and BTC. Amounts can be specified with exact values, bounds or vague descriptions. The answer must fit the schema."

# r = create(
#     messages=[
#         {
#             "role": "system",
#             "content": prompt,
#         },
#         {
#             "role": "user",
#             "content": "I want to buy 10 ETH for 2 BTC with anyone before tomorrow 8am"
#         },
#     ],
#     response_model=IntentModel,
# )
while True:
    data = input("> Enter trade description\n")
    if data == "q":
        break
    r = create(messages=[ { "role": "system", "content": prompt },
                          { "role": "user", "content": data }
                         ],
               response_model=TransferModel
        )
    print(r)


# I want to send 15 ethereum and 2 bitcoin to nacl:0xaa04d11fa77e57490abc45e1af35b37cf8525af9577a29fbdeabbc000081a4c237152046eb9d9790c44391f4a18f1b07
