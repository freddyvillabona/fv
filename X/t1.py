import tweepy

# 1. TUS CLAVES DE OAUTH 2.0 (De las capturas anteriores)
CLIENT_ID = "Z3JYTktpdUFDVFUzKz1NThWWkc6MTpjaQ"
CLIENT_SECRET = "N3O0iVWicapMmQYG51R5qFZ9ZVijGSU6BPN7aFkTXFo7qmSIXN"

# 2. CONFIGURACIÓN CON TU URL REAL DE GITHUB
oauth2_user_handler = tweepy.OAuth2UserHandler(
    client_id=CLIENT_ID,
    client_secret=CLIENT_SECRET,
    redirect_uri="https://github.io", # <--- CAMBIADO AQUÍ
    scope=["tweet.read", "tweet.write", "users.read"]
)

# 3. OBTENER EL ENLACE DE AUTORIZACIÓN
auth_url = oauth2_user_handler.get_authorization_url()
print("👉 PASO A: Abre este enlace en tu navegador para autorizar tu cuenta:")
print(auth_url)
print("\n" + "="*50 + "\n")

# 4. INTRODUCIR LA URL DE RETORNO
redirect_response = input("👉 PASO B: Después de autorizar, copia la URL completa de la barra de direcciones y pégala aquí: ")

# 5. GENERAR EL CLIENTE Y ENVIAR TUIT
try:
    access_token = oauth2_user_handler.fetch_token(redirect_response)
    client = tweepy.Client(access_token=access_token["access_token"])
    
    response = client.create_tweet(text="¡Bot de X funcionando correctamente con OAuth 2.0!", user_auth=False)
    print("\n✅ ¡Tuit enviado con éxito!")

except Exception as error:
    print("\n❌ Error durante el proceso:", error)

