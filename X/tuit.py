import tweepy

# 1. Configura tus credenciales (OAuth 1.0a User Context)
# Se obtienen del portal de desarrolladores de X
# token de act NG4yS0hUNjJWQ1ZqWVZRM25YbHNlbzNjUXZvTUI0Sm5pSThxX2FNN1k1Q2cwOjE3ODYyMTExNjI1Mzk6MTowOnJ0OjE
CONSUMER_KEY = "95rmiHKdYiZDdHWxZdyvYH6x5"
CONSUMER_SECRET = "HlthZuFMzus1yWyz7cDlsa8AxShrdbCKJciuFrVDc9ieDuiavw"
ACCESS_TOKEN = "Z3NqVURESFVnMG1pR29ZY252SHZnR2kyZFlGWlZwQUpFazBfbEFRVWFELWtFOjE3ODYyMTEzNTU3NTE6MTowOmF0OjE"
ACCESS_TOKEN_SECRET = "D-0IF-NXhqZSTMuoLO_4FfXzrqF0eqT-sAgIIZdWnthXu4BNNp"

# 2. Inicializar el cliente de la API v2
# Este método requiere obligatoriamente los 4 parámetros para publicar
cliente = tweepy.Client(
    consumer_key=CONSUMER_KEY,
    consumer_secret=CONSUMER_SECRET,
    access_token=ACCESS_TOKEN,
    access_token_secret=ACCESS_TOKEN_SECRET
)

# 3. Define el mensaje de tu tuit
mensaje = "¡Hola Mundo! Tuiteando automáticamente desde Python. 🐍🚀"

# 4. Enviar el tuit protegiendo el código contra errores
try:
    respuesta = cliente.create_tweet(text=mensaje)
    # Extraemos el ID del nuevo tuit para confirmar el éxito
    tuit_id = respuesta.data['id']
    print(f"✅ ¡Tuit enviado con éxito!")
    print(f"🔗 ID del tuit: {tuit_id}")
    print(f"🌐 Puedes verlo en: https://x.com{tuit_id}")

except tweepy.TweepyException as e:
    print(f"❌ Error de Tweepy al enviar el tuit: {e}")
except Exception as e:
    print(f"❌ Ocurrió un error inesperado: {e}")
