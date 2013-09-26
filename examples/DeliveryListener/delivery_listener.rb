require 'rubygems'
require 'sinatra'
  
post '/' do
  puts params
  #status 416
  "Hello"
end
