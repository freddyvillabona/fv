import tweepy

# CONFIGURACIÓN CORRECTA
client = tweepy.Client(
    consumer_key="95rmiHKdYiZDdHWxZdyvYH6x5",
    consumer_secret="HlthZuFMzus1yWyz7cDlsa8AxShrdbCKJciuFrVDc9ieDuiavw",
    access_token="Z3NqVURESFVnMG1pR29ZY252SHZnR2kyZFlGWlZwQUpFazBfbEFRVWFELWtFOjE3ODYyMTEzNTU3NTE6MTowOmF0OjE",
    access_token_secret="D-0IF-NXhqZSTMuoLO_4FfXzrqF0eqT-sAgIIZdWnthXu4BNNp"
)

# MÉTODO CORRECTO PARA ENVIAR EL TUIT
try:
    response = client.create_tweet(text="Mensaje de prueba automatizado")
    print("¡Tuit enviado con éxito!")
except tweepy.TweepyException as e:
    print(f"Error: {e}")
