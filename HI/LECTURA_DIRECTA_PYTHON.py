import requests
import pandas as pd

# URL del archivo Excel
url = "https://datosabiertos.gob.ec/dataset/0ec65ab4-e6ab-40ab-aab9-c91e912f9faf/resource/cb8f704e-2b27-4d7f-9431-d40c4e27fa48/download/mdi_homicidiosintencionales_pm_2024_enero-septiembre.xlsx"

# Descargar el contenido del archivo directamente en memoria
response = requests.get(url)

# Leer el archivo Excel directamente desde la memoria usando pandas
# Usamos un objeto BytesIO para leer desde la respuesta
from io import BytesIO
excel_data = pd.read_excel(BytesIO(response.content))
