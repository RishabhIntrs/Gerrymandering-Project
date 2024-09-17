import random
import numpy as np
from collections import Counter, defaultdict
import string
import re

# Given encoded message
encoded_message = (
    "ziatwxafbssuladwafb uanubeablwdfunaywwmabywxdakfnzgdwsfunan"
    "wyzlatwxarbtanururyunadfbdafuawlkuafbeabagoblawnadfuagoblaf"
    "beakfnzgdwsfunanwyzlazaewldamlwoaofzkfableadfbdafuaxgueadwa"
    "kbjjadfzgagoblaswwfaadfbdaobgabajwlhadzruabhwableaofulaouag"
    "bzeahwweytuaouadwwmadfualbruaozdfaxgabgaouaezeldadfzlmadfua"
    "goblaowxjeaobldazdabltarwnuaoujjaofulaueobneayubnagbzeadfbd"
    "afuaowxjeajzmuablauqkzdzlhalbruabjjadwafzrgujiakfnzgdwsfuna"
    "nwyzlagbzeabdawlkuaozdfwxdagdwsszlhadwadfzlmadfbdafuaobgaoz"
    "llzudfuswwfableafuaobgagwabgazafb uauqsjbzlueadfuaswwfasbnd"
    "azaozjjalwoauqsjbzladfuanugdawiazd"
)

with open('//Users//rishabhsinha//Documents//pg2600.txt') as file:
    wap = file.read()

def remove_special_characters(string):
    return re.sub(r'[^a-zA-Z\s]', '', string)


# Compute bigram frequencies
def bigram_frequencies(text):
    formatted_text= remove_special_characters(text)
    bigrams = Counter(zip(formatted_text, formatted_text[1:]))
    total_bigrams = sum(bigrams.values())
    bigram_freq = {bg: count / total_bigrams for bg, count in bigrams.items()}
    return bigram_freq

encoded_bigrams = bigram_frequencies(encoded_message)
english_bigrams = bigram_frequencies(wap)

# Decode message with a given permutation
def decode_message(encoded_message, perm_map):
    return ''.join(perm_map[c] for c in encoded_message)

# Calculate plausibility based on bigram frequencies
def plausibility(decoded_message, english_bigrams):
    bigrams = zip(decoded_message, decoded_message[1:])
    log_P = 0
    for bigram in bigrams:
        log_P += np.log(english_bigrams.get(bigram, 1e-6)) #ignoring bigrams 2 small
    return log_P

# MCMC Simulation
def mcmc(encoded_message, english_bigrams, iterations=12000):
    alphabet = list(string.ascii_lowercase + ' ')
    random_permutation = alphabet[:]
    random.shuffle(random_permutation)
    perm_map = dict(zip(alphabet, random_permutation))
    
    best_perm = perm_map
    best_plausibility = plausibility(decode_message(encoded_message, perm_map), english_bigrams)
    
    for _ in range(iterations):
        # Randomly swap two characters in the permutation
        new_perm = perm_map.copy()
        a, b = random.sample(alphabet, 2)
        new_perm[a], new_perm[b] = new_perm[b], new_perm[a]
        
        new_decoded_message = decode_message(encoded_message, new_perm)
        new_plausibility = plausibility(new_decoded_message, english_bigrams)
        
        if new_plausibility > best_plausibility:
            perm_map = new_perm
            best_plausibility = new_plausibility
            best_perm = perm_map
        else:
            if random.random() < np.exp(new_plausibility - best_plausibility):
                perm_map = new_perm
                best_plausibility = new_plausibility
                best_perm = perm_map
            
    return best_perm

# Run the MCMC simulation
best_permutation = mcmc(encoded_message, english_bigrams)

# Decode the message with the best permutation
decoded_message = decode_message(encoded_message, best_permutation)
print("Decoded Message:")
print(decoded_message)
