import System.Random
import System.IO.Unsafe
randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))	
	



users=["user1","user2","user3","user4"]
items=["item1","item2","item3","item4","item5","item6"]
purchasesHistory=[("user1",[["item1","item2","item3"],["item1","item2","item4"]]),
					("user2",[["item2","item5"],["item4","item5"]]),
					("user3",[["item3","item2"]]),
					("user4",[])]
	

				
createEmptyFreqList [] =[] 					
createEmptyFreqList (x:xs) = (x,[]):createEmptyFreqList xs  	


-- to flatten list of lists into a single list 
flatten l = foldr (++) [] l 	
			
-- to remove duplicates 
removeDups []=[]
removeDups (x:xs)|elem x xs = removeDups xs 
				 |otherwise = x:removeDups xs 

-- to get to the second element of a pair in a list of pairs 
getSecOfPair first ((x,y):xs)|x== first = y
							 |otherwise= getSecOfPair first xs 

-- takes an item and a list containing the item and returns a list of all other items 							 
getOtherItems item []=[]
getOtherItems item (x:xs)|item == x = getOtherItems item xs 
					     |otherwise = (x:(getOtherItems item xs)) 
							 
-- takes an item and a list of lists and returns a list of lists containing accompanying items 							 
getAccompanyingItems _ [] = [] 							 
getAccompanyingItems item (x:xs) | elem item x = (getOtherItems item x ):getAccompanyingItems item xs
								 | otherwise = getAccompanyingItems item xs 
								 
--removes all occurences of an item from a list 
removeAllOccurences _ []=[] 								 
removeAllOccurences item (x:xs)|x==item = removeAllOccurences item xs 
							   |otherwise = x:removeAllOccurences item xs 
							   
-- to count the number of occurences of an item in a list 
countOccurences _ []=0 
countOccurences item (x:xs)| x==item= 1+countOccurences item xs 
						   | otherwise = countOccurences item xs
						   
-- takes list of FLATTENED accompanying items of a certain item and returns list of frequencies		   
createFreq [] =[] 							 
createFreq (item:xs)= (item,(countOccurences item xs)+1):createFreq(removeAllOccurences item xs) 							 
					
getAllUsersStats []=[] 					
getAllUsersStats ((user,list):xs)= (user,getAllUsersStatsHelper items list) :getAllUsersStats xs 

-- creates list per user 
getAllUsersStatsHelper [] _ =[] 
getAllUsersStatsHelper (item:xs) list = (item,createFreq(flatten(getAccompanyingItems item list))):getAllUsersStatsHelper xs list 

-- to get the  list of second elements of each pair from a list of pairs 
getSecond [] =[]
getSecond ((x,y):xs)=y++getSecond xs

--takes user and allUserStats and returns the frequency lists
getListOfItemFreq _ []=[] 
getListOfItemFreq userName ((user,listOfPairs):xs)|userName==user=getSecond listOfPairs   					
												  | otherwise = getListOfItemFreq userName xs
												  
--takes an item and freq list produced by the above function and returns total frequency												  
countFreqPerItem _ []=0 
countFreqPerItem itemName ((item,freq):xs)| item==itemName = freq + countFreqPerItem itemName xs 
										  | otherwise = countFreqPerItem itemName xs 
										  
--takes an item and a list of pairs and removes all occurences where first element of pair is equal to the item										  
removeAllOccurencesfromPair _ []=[] 
removeAllOccurencesfromPair itemName ((item,freq):xs)|itemName==item= removeAllOccurencesfromPair itemName xs 
													 |otherwise = (item,freq):removeAllOccurencesfromPair itemName xs 
					
-- takes list produced by getListOfItemFreq and returns freqListItems 				
createFreqListItems []=[] 										
createFreqListItems ((item,freq):xs)= (item,(countFreqPerItem item xs +freq)):createFreqListItems(removeAllOccurencesfromPair item xs)										  
										  
freqListItems user = createFreqListItems (getListOfItemFreq user (getAllUsersStats purchasesHistory))										  
										  
-- takes item and finds the frequency list of items from user stats
itemListFinder _ []=[]
itemListFinder itemName ((item,list):ys)|itemName==item = list
											|otherwise= itemListFinder itemName ys

-- appends the results of the above function for all items in the cart 											
freqListCartHelper [] _ =[]											
freqListCartHelper (item:xs) list = itemListFinder item list ++ freqListCartHelper xs list										  
										  
freqListCart user cart = createFreqListItems(freqListCartHelper cart (getSecOfPair user (getAllUsersStats purchasesHistory)))

freqListCartAndItems user cart = createFreqListItems (freqListCart user cart++freqListItems user)										  

exists itemName [] = False					
exists itemName ((item,list):xs)| itemName==item&& list/=[]= True 
								|otherwise=  exists itemName xs 
purchasesIntersectionHelper [] _=[]									  
purchasesIntersectionHelper ((item,freqList):xs) (user,list) | freqList/=[]&&exists item list =(item,(freqList++getSecOfPair item list)):purchasesIntersectionHelper xs (user,list)
																  | otherwise = purchasesIntersectionHelper xs (user,list) 
										  
purchasesIntersectionHelper2 l= map(\(item,freqList)->(item,(createFreqListItems freqList))) l 

purchasesIntersection _ []=[] 
purchasesIntersection userList (usersStats:xs) = purchasesIntersectionHelper2 (purchasesIntersectionHelper userList usersStats):(purchasesIntersection userList xs) 
									  
									  
freqListUsers user = createFreqListItems (getSecond(flatten(purchasesIntersection (getSecOfPair user (getAllUsersStats purchasesHistory)) (removeAllOccurencesfromPair user (getAllUsersStats purchasesHistory))))) 									  
									  		

											
--recommendEmptyCart user =item!!(randomZeroToX (length list)) where{ elem (item,freq) freqListItems user}

getFirstofPair (x,y)= x
recommendEmptyCartHelper [] =[]
recommendEmptyCartHelper list=(list !!(randomZeroToX((length list)-1))) 

recommendEmptyCart user = recommendEmptyCartHelper (repeatItemsInAList(freqListItems user)) 


recommendBasedOnItemsInCart user cart| getSecOfPair user purchasesHistory ==[]=[]
									 |otherwise = recommendBasedOnItemsInCartHelper (repeatItemsInAList(freqListCartAndItems user cart))


recommendBasedOnItemsInCartHelper []=[] 
recommendBasedOnItemsInCartHelper list =(list !!(randomZeroToX((length list )-1))) 


recommendBasedOnUsers user = recommendBasedOnUsersHelper (repeatItemsInAList(freqListUsers user))
recommendBasedOnUsersHelper []=[]
recommendBasedOnUsersHelper list = (list !!(randomZeroToX((length list )-1))) 

repeatItemsInAList []=[] 
repeatItemsInAList ((item,freq):xs)|freq>0=item:repeatItemsInAList ((item,freq-1):xs) 
								   | otherwise = repeatItemsInAList xs 
								   
								   
recommend user cart | (recommendHelper (recommendBasedOnItemsInCart user cart) (recommendBasedOnUsers user))==["",""]=(items !!(randomZeroToX((length items)-1))) 
				    | otherwise = ((recommendHelper (recommendBasedOnItemsInCart user cart) (recommendBasedOnUsers user)) !!(randomZeroToX((length (recommendHelper (recommendBasedOnItemsInCart user cart) (recommendBasedOnUsers user)) )-1))) 

recommendHelper basedOnCart basedOnUsers = [basedOnCart,basedOnUsers]						   

