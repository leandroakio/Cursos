# creating the API -> Flask
from flask import Flask, request

# libs
from datetime import datetime
import joblib
import sqlite3

# instancing the application
Aplicativo = Flask(__name__)
#-------------LOADING THE MODEL--------------------
# loading the model
Modelo = joblib.load('Modelo_floresta_aleatorio_v100.pkl')

#------------API FUNCTION---------------------------
# function to receive the API
@Aplicativo.route('/API_Preditivo/<area>;<rooms>;<bathroom>;<parking_spaces>;<floor>;<animal>;<furniture>;<hoa>;<property_tax>', methods=['GET'])
def Funcao01(area, rooms, bathroom, parking_spaces,floor,animal,furniture,hoa,property_tax):

    #Datetime of login at API
    Data_inicio = datetime.now()



    # receiving inputs from API
    Lista = [float(area), float(rooms), float(bathroom), float(parking_spaces),float(floor),float(animal),float(furniture),float(hoa),float(property_tax)]

    # calculating the prediction
    try:
        #predict
        Previsao = Modelo.predict([Lista])
        #insert Previsao value
        Lista.append(str(Previsao))

        # transform the list in string
        Input = ''
        for Valor in Lista:
            Input = Input + ';' + str(Valor)

        #end of process at API
        Data_fim = datetime.now()
        Processamento = Data_fim - Data_inicio

        #-------------CONNECTING WITH DATABASE--------------
        # creating the connection with Database_API
        Conexao_Banco = sqlite3.connect('Database_API.db')
        Cursor = Conexao_Banco.cursor()

        #Query
        Query_insert = f'''
            INSERT INTO Log_API (Input, Login, Logout, Processamento)
            VALUES ('{input}', '{Data_inicio}','{Data_fim}','{Processamento}')
        '''

        #Execute query
        Cursor.execute(Query_insert)
        Conexao_Banco.commit()
        Cursor.close()

        #returned message
        return {'Rent_value' : str(Previsao)}

    except:
        return{'Alert':'Unexpected error!'}

if __name__ =='__main__':
    Aplicativo.run(debug=True)