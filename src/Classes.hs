module Classes 
where 

--import RIO 

import Config 


class HasConfig a where 
  getConfig :: a -> Config 
