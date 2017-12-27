% To generate the sml lists for the choose your own adventure
%   type makeMessage('adventure.txt', 'output.txt', .5)
function [] = makeMessage(infile, outfile,prob)

  % Read in the CSV file infile where the first column is the list name
  % and the second column is the message.
  inID = fopen(infile,'r');
  data = textscan(inID, '%s %s','Delimiter',';' );
  listname = data{1};
  messages = data{2};

  outid = fopen(outfile,'w');
  language  = {'f','u','n';'a','r','e';'v','a','l'}';
  symbols = ['a':'z','A':'Z','0':'9',' '];
  numsym = length(symbols);

  helpmessage = "You are off by 1. ";

for index = 1: length(listname)
  lname = listname{index}
  message = messages{index};
  count = 1;
  start = true;
  fprintf(outid, ['val ',listname{index},' = ']);
  fprintf(outid,'[');
  while count <=length(message)
    r = rand(1);
    if start
       fprintf(outid,'\"');
       start = false;
    else
       fprintf(outid,',\"');
    end
    if (r < prob )
      mchars = randi(3,4,1);
      if (count <= length(helpmessage))
	fprintf(outid,[language{mchars(1),1},language{mchars(2),2},...
		     language{mchars(3),3},symbols(randi(numsym)), ...
		     message(count),helpmessage(count),...
		     symbols(randi(numsym)),symbols(randi(numsym)),...
		     symbols(randi(numsym)),symbols(randi(numsym)),...
		     '\"']);
      else
	fprintf(outid,[language{mchars(1),1},language{mchars(2),2},...
		     language{mchars(3),3},symbols(randi(numsym)), ...
		     message(count),symbols(randi(numsym)),...
		     symbols(randi(numsym)),symbols(randi(numsym)),...
		     symbols(randi(numsym)),symbols(randi(numsym)),...
		     '\"']);
      end
      count = count +1;
    else
      rchars = symbols(randi(numsym,10,1));
      for i = 1:3
	if rchars(i) == language{1,i} ||  rchars(i) == language{2,i} ...
	      ||  rchars(i) == language{3,i}
	  rchars(i) = upper(rchars(i));
	  end
      end
      fprintf(outid,[rchars,'\"']);
    end
  end

  fprintf(outid,']\n');
end
  fclose(outid);
end
