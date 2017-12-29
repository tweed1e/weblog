---
title: A dog diet base rate fallacy
author: "Jesse Tweedle"
date: '2017-12-29'
slug: a-dog-diet-base-rate-fallacy
categories: ["dogs", "stats"]
tags: ["dogs", "diet", base rate", "fallacy"]
description: 'There are 70 million dogs in the US. Those dogs on raw food diets are much more likely to cause salmonella infections in humans than on dry (kibble) food diets.'
image: "https://jesse.tw/images/circle-arrows.png"
---

# :dog:>:poop:

I have a dog. He's cute and crazy. And he has constant diarrhea (shout out to the pet parents and human baby parents out there, you get used to talking about :poop: real quick). It turns out part of the reason was his diet.

The vet just shrugged when I asked how to fix it. Google suggested raw diets like RMB [raw meaty bones](http://www.rawmeatybones.com/petowners/feedyourdogrmb.php) or BARF [bones and raw food, or biologically appropriate food](http://www.barfworld.com/). A raw diet means you put an uncooked whole chicken in a bowl and the dog eats it. It's great. They just know what to do. A dog won't get sick from salmonella like a human will, and they can eat uncooked chicken bones safely (except a certain size chicken neck can choke them if they don't chew it).

Buuuut you're handling raw chicken in a way that you're not used to, and that's not as safe. There's now raw chicken on your floor if the dog picks it up and moves it. There's raw chicken on his mouth for him to rub on everything in your house. But a few pro-raw food websites claim raw diets are as safe as dry food diets.

> So while both raw food and kibble contain traces of salmonella and E. coli, it is more likely for the kibble fed animal to become infected with salmonella or E. coli because they are not as healthy and not as able to pass the bacteria through their systems. [www.truecarnivores.com](https://www.truecarnivores.com/raw-food-contain-salmonella-e-coli-will-feeding-raw-food-make-pet-sick/)

And:

> The fact is the majority of human salmonellosis cases are acquired through ingestion or handling of contaminated dry pet foods and treats – not raw meat. [healthypets.mercola.com](https://healthypets.mercola.com/sites/healthypets/archive/2013/04/08/raw-food-diet-part-2.aspx)

Which is used as evidence that dry food is as bad as raw food. It's true that dry food is sometimes contaminated by salmonella or other things (see [CDC salmonella outbreak from dry food, 49 individuals](https://www.cdc.gov/salmonella/dog-food-05-12/)), but this is a question of magnitudes.

## Evaluate the :poop: out of these claims

Is it "more likely for the kibble fed animal to become infected with salmonella"? What does it mean that "the majority of human salmonellosis cases are acquired through...dry pet foods?"

There were a few studies on dry vs raw food contamination published in the Canadian Veterinary Journal, in [2002](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC339295/) and [2011](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3003575/). 

> Salmonella was isolated from 80% of the BARF diet samples (p < 0.001)

Ok, there were 10 dogs in control and 10 in treatment, which is not a lot, but 8/10 in the BARF diet had salmonella cultures and 0 in the control group (dry food only). So that first claim is struck off---the raw fed animal is much more likely to become infected with salmonella.

Is it then true that the majority of human salmonellosis cases are acquired through dry pet foods? 

## Consider the base rate

* there were 69,926,000 dogs in the US in 2012, according to the [American Veterinary Medical Association](https://www.avma.org/KB/Resources/Statistics/Pages/Market-research-statistics-US-pet-ownership.aspx). 
* 49 people were infected by salmonella in dry food that year

We want to compare these two:

```
dry_rate = num. salmonella cases from dry diets / num. dry diets
raw_rate = num. salmonella cases from raw diets / num. raw diets
```

So we need to divide the 69 million dogs into dry and raw diets. My personal estimate of this is: I tried raw for about 2 weeks this year, which makes me 3.8% of a raw diet. I know about 99 other dogs, none of whom have tried raw diets. Which makes the rate of raw diets about 0.038%. This is the key. Any comparison of salmonella rates across diets needs to account for the base rate of raw diets in the population (which is so low that I don't know anyone that uses it, not even myself for more than a couple of weeks). 

```
dry_rate = 49 / (0.99962 x 69926000)
raw_rate = X / (0.00038 x 69926000)
```

The `dry_rate` is so low you wouldn't notice. Suppose there was exactly one case of salmonella due to a raw diet in 2012, so `X = 1`. Then `raw_rate / dry_rate = 2630.` Even if the number of raw-salmonella cases is much lower than the number of dry-salmonella cases (which makes the above claim true), the rate of infection from raw diets is more than 2000x higher (making the claim true, but meaningless).

[PSA: do not feed your dog **cooked** chicken bones.]

# Raw diets suck :poop: bye.

