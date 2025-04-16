# MasterMindMaMeglio Mathematica - Gioco con Quiz Integrato

Un clone del classico gioco **MasterMind** sviluppato in *Wolfram Mathematica*, con meccaniche ispirate a Wordle e un sistema di suggerimenti basato su quiz di cultura generale.

## Come Funziona

- **Gioco Base**: Indovina la sequenza di colori segreta entro un numero limitato di tentativi.
- **Feedback**: I pin laterali indicano:
  - ✅ **Esatto**: Colore corretto e posizione giusta.
  - 🔄 **Spostato**: Colore corretto ma posizione sbagliata.
  - ❌ **Assente**: Colore non presente nella sequenza.
- **Quiz Bonus**: Rispondi a domande di trivia per ottenere suggerimenti!

## Divisione del Lavoro

### Segna con una X i task completati

### 1. Database Quiz Trivia

- [ ] Creare un database di domande/risposte.
- [ ] Categorizzare le domande per difficoltà.
- [ ] Integrare un sistema di punteggio per i suggerimenti.

### 1.5. Interfaccia Quiz

- [ ] Design della finestra pop-up per le domande.
- [ ] Collegamento tra risposte corrette e suggerimenti di gioco.

### 2. Interfaccia Grafica

- [ ] Disegnare la griglia di gioco con `Graphics`/`DynamicModule`.
- [ ] Implementare i pin di feedback laterali (stile Wordle).
- [ ] Creare pulsanti per invio tentativi e aiuto.

### 3. Documentazione

- [x] Scrivere il `README.md` (questo file).
- [ ] Manuale utente in notebook Mathematica (`.nb`).
- [ ] Commentare il codice per spiegare le funzioni.

### 4. Logica MasterMind

- [x] Generare sequenze segrete casuali.
- [x] Implementare il sistema di confronto tentativo/segreta.
- [x] Gestire il conteggio dei tentativi e la vittoria/sconfitta.

### 4.5. Gestione Colori

- [x] Definire la palette di colori giocabili.
- [ ] Assegnare simboli/icone per i feedback (✅/🔄/❌).

### 5. Input Utente

- [ ] Rilevare clic su griglia per selezionare colori.
- [ ] Validare gli input prima dell'invio.
- [ ] Aggiungere suoni/animazioni per l'interazione.

### 6. Variabili e Varianti

- [ ] Implementare livelli di difficoltà (lunghezza sequenza).
- [ ] Opzioni personalizzabili?

### 7. Extra & Debug

- [ ] Testare il gioco e correggere bug.
- [ ] Aggiungere Easter eggs o temi grafici.
- [ ] **Pizze 🍕**

## Contributi

**Team di sviluppo:**

- Frigo (@fri3erg) - _  
- Matte (@matteraggi ) - _  
- Francesca (@francesca452) - _  
- Angelo (@Lasagnos) - _  
- Gianpiero (@gianpics) - _  
- Alessandro (@alessandromodelli) - __
