# Eficiencia
[] Usar diskin para samples largos o que se ejecutan de corrido
[] Sistema para liberar samples largos que se usan como slices de un canon

# Prototipar
[] Secuenciador

# Mejorar
[x] Pasar el `state` al `synth*` y agregar un `params` al spec del nodo para pasar al synth
[x] Para poder modificar un canon durante la secuencia. Crear función `get-param` que lea el param del nodo actual o ejecute una funcinó default si el param es nil. Los params son un atomo con un mapa.
[x] Agregar specs para grafos y estado
[] Agregar función que explique parametros que se pasan a los synth*
[] Mejorar specs para no requerir usar var-get o deref

# Canones/graphos/synthdefs
[] Flocking: usar transps (rate) para generar efectos de flocking donde diversas voces puedan ir modificando la manera en que se transporta un canon. Y para controlar paneos también.


# Audios
[] Buscar audios para aves (volando y cantando, y etc?).
