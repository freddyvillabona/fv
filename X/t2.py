import requests

access_token = "ZmhhWHZMNmFGTk9RWi1kYld0Q3ZHc004YzV4UW0tdUhRR1RuUE5vQlA4Mk1BOjE3ODYyMjA2MTYxMDA6MTowOmF0OjE"  # el que dice "Para @FreddVillabona"

url = "https://api.twitter.com/2/tweets"
headers = {
    "Authorization": f"Bearer {access_token}",
    "Content-Type": "application/json"
}
payload = {"text": "¡Hola mundo desde Python! 🐍"}

response = requests.post(url, json=payload, headers=headers)
print(response.status_code, response.json())
