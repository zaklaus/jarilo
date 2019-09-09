pico-8 cartridge // http://www.pico-8.com
version 18
__lua__
-- jarilo - demo code
-- created by zaklaus 2019

local dbg=0

-- fill out tile mappings
tiles={}

--[[

sprite bank layout:
	1-2 tabs are used for textures/sprites
	3rd tab is used by the engine
	4th tab is used for map tiles

--]]

-- this is where our tileset starts
-- negative numbers specify passable block
-- format: tiles[tileid]={t=spriteid,h=wallheight,d=isdoor}
-- tileid is an id of tile we use in map editor
-- sprite id points to a 16x16 texture representation
-- in our sprite bank.
tts=192
tiles[tts+0]={t=0,h=1,d=false}
tiles[tts+1]={t=-1}
tiles[tts+2]={t=-2}
tiles[tts+3]={t=3,h=1,d=true}
tiles[tts+4]={t=4,h=1}
tiles[tts+5]={t=5,h=1}
tiles[tts+6]={t=6,h=1}
tiles[tts+7]={t=7,h=1,d=true}
tiles[tts+8]={t=17,h=2}
tiles[tts+9]={t=16,h=2}

indoors={
	{
		x1=2,y1=2, -- x1,y1
		x2=7,y2=6, -- x2,y2
		sky=5,     -- sky color
		gnd=1,     -- ground color
		shdt=4,    -- shading factor
		shdf=15,    -- shading far
		shdn=15,   -- shading near
		shds=0,    -- shading side
	},
	{
		x1=20,y1=0, -- x1,y1
		x2=27,y2=15, -- x2,y2
		sky=5,     -- sky color
		gnd=4,     -- ground color
	},
	
}

-- game settings
mysettings={
	-- disable jarilo startup banner
 nobanner=dbg,
 
 -- display top-down map
 draw2d=0,
 
 -- assigns our tile mappings
 mapstart=tts,
 mapping=tiles,
 
 -- environment settings
 skycolor=12,
 groundcolor=3,
 nodither=false,
 nofloor=false,
-- ditheramt=1,
 
 -- list of zones with special shading properties
 zones=indoors,
 
 -- disable shading
 fullbright=0,
 
 -- set up ambience
 ambfactor=1.12, -- shading distance falloff factor
 ambfar=4,    -- distant wall shading
 ambnear=15,   -- near wall shading
 ambside=2,   -- side wall shading
}

function _init()
 cls()

	mysettings.nobanner=dbg
 j_init(mysettings)
 
 if dbg==false then
	 cls()
	 print"welcome!"
	 print"arrow keys to move"
	 print"z to toggle 2d/3d map"
	 wait(80)
	 cls()
	end

 plr:setpos(6, 5.5)
 plr:turn(329/360)
end

function _update()
 j_update()
 
 local rot=6

 if btn(0) then
  plr:turn(-rot)
 end

 if btn(1) then
  plr:turn(rot)
 end

 local sp=0.16

 if btn(2) then
  plr:move(sp)
 end
 if btn(3) then
  plr:move(-sp)
 end

 -- temp
 if btnp(4) then
  settings.draw2d = 1-settings.draw2d
 end
 
 if btnp(5) then
 	dbg=not dbg
 end
end

-- used for sky
--[[ function j_predraw()
	for y=scrh/2,scrh/4,-1 do
		for x=scrw,0,-1 do
			local camx=x/scrw+plr.angle/360/3
			local py=y+sin(camx)*4
			
			if py<scrh/2 then
				pset(x,py,7)
			end
		end
	end
end --]]

function _draw()
 calcfps(time())
 j_draw()
 
 if dbg==1 then
	 print("fps "..fps,2,2,7)
	 print("cpu: "..flr(stat(1)*100).."%",2,10,7)
 end
end

-->8
-- jarilo - 3d game engine
-- by zaklaus 2019

--[[
	todo:
		[ ] sprite support
		[x] better per-zone shading support
		[ ] taller walls
		[ ] basic elevation
		[ ] head bobbing 

--]]

-- core

plr={}
local _world={}
dist_tbl={}
detlim_tbl={}
settings={}
local map2tex_map={}
local tex2map_map={}

function j_init(s)
 s=s or {}

 if s.nobanner~=1 then
  cls()
  print"jarilo engine 1.0"
  print"created by zaklaus 2019"
  wait(40)
 end

 scrw=s.scrw or 128
 scrh=s.scrh or 128
 s.mapstart=s.mapstart or 192
 s.skycolor=s.skycolor or 0
 s.groundcolor=s.groundcolor or 1
 s.nodither=s.nodither or false
 s.ditheramt=s.ditheramt or 1
 s.nofloor=s.nofloor or false
 s.zones=s.zones or {}

 s.ambfactor=s.ambfactor or 2
 s.ambfar=s.ambfar or 7
 s.ambnear=s.ambnear or 15
 s.ambside=s.ambside or 3

 settings=s
 plr:new()

 for y=0,scrw do
  dist_tbl[y]=scrw/(2*y-128)
  detlim_tbl[y]=flr(98+7*sin(y/256))
 end

 map2tex_map=s.mapping
 _world=genworld()
 _world.zones = s.zones
 genpal()

 for y=1,#_world.map do
  for x=1,#_world.map[y] do
   poke(0x4300+y*32+x,abs(_world.map[y][x].t))
  end
 end
 
 for i,m in pairs(map2tex_map) do
 	tex2map_map[m.t]=m
 end

 timer=0

 statusbar=""
end

function j_update()
 timer+=1

 update_doors(_world)
end

function j_draw()
 pal_reset()

 if settings.draw2d == 1 then
  draw_2d(_world)
 else
  draw_world(_world)
 end

 print(statusbar, 2, 100, 14)
end

-- renderer

function draw_2d(world)
 cls(5)
 pal(7,7)
 camera(plr.pos.x*8-64,
 							plr.pos.y*8-64)
 pal_reset()

 for y=1,#world.map do
  for x=1,#world.map[y] do
   local id=world.map[y][x].t

   if id~=nil then
    local sp=abs(id*2)
    spr(sp,(x-1)*8,(y-1)*8)
   end
  end
 end

 local px=(plr.pos.x-1)*8
 local py=(plr.pos.y-1)*8
 local pxe=px+plr.dr.x*8
 local pye=py+plr.dr.y*8

 for x=1,128 do
  local pos,ray,mapx,sx,mapy,sy,tex,side,rxe2,rye2,_=castat(world,plr,x,scrw)
  if tex==settings.doorid then
   if side==0 then
    mapx+=sx/2
   else
    mapy+=sy/2
   end
  end

  rxe2*=8
  rye2*=8

  rxe2-=8
  rye2-=8

  line(px,py,rxe2,rye2,9)
  pset(rxe2,rye2,14)
 end

 line(px,py,pxe,pye,8)
 pset(px,py,9)

 camera()
end

function draw_world(world)
 -- set up values
 local floors={}
 local zbuf={}
 local check=0
 local prevside=1
 local posx=plr.pos.x
 local posy=plr.pos.y 
 
 cls(settings.skycolor)
 rectfill(0,scrh/2,scrw,scrw,settings.groundcolor)
 
 if j_predraw~=nil then
  j_predraw()
 end

 for x=0,scrw do
		local pwalld,ray,side,tex,mapx,mapy,rxe2,rye2,d=raycast(world,plr,x,scrw)
	 local shdn=settings.ambnear
	 local shdf=settings.ambfar
	 local shds=settings.ambside
	 local shdt=settings.ambfactor
  local skyc=settings.skycolor
  local gndc=settings.groundcolor
		local az=zonef(rxe2,rye2)
		
		pal_reset()
		
		if az~=nil then
	 	shdn=az.shdn or shdn
	 	shdf=az.shdf or shdf
	 	shds=az.shds or shds
	 	shdt=az.shdt or shdt
	 	skyc=az.sky  or skyc
	 	gndc=az.gnd  or gndc
	 end
	 		
  zbuf[x] = pwalld

  local lh=flr(scrh/pwalld)
  local hh=lh/2
  local stacks=0
  local dstart=-(hh)+scrh/2
  local dend=flr(hh+scrh/2)

  if dstart>=dend then
   dstart=0
   dend=scrh
  end

  if (dend<(scrh/2)) dend=scrh

  local wallx=0

  if side==0 then
   wallx=posy+pwalld*ray.y
  else
   wallx=posx+pwalld*ray.x
  end

  wallx-=flr(wallx)

  -- calculate texture index
  local texx=flr(wallx*16)
  if d and tex==settings.doorid then
   if d.oval>0 then
    texx+=1
   end
   texx-=(d.oval/100)*16
  end

		local p=15
		
  if settings.fullbright~=1 then
   local dcol=
   	min(15,
   					flr(
   						(dend-dstart)
   							/shdt))
   if side==1 then
   	dcol=
   		max(dcol-shds,0)   	
   end
   p=
   	clamp(15-dcol,
   		15-shdn,
   		shdf)
  end

  -- draw wall
  local vp=((scrh/2)-dstart)*2
  local t=0
  for i=0,stacks do
   local a=hh*(max(1,i*3))
   local dstart2=-a+(scrh/2)
   local texl=0
   tex=copy(tex)
		 while tex.t>=16 do
   	tex.t-=16
   	texl+=1
   end
  
  	if d~=nil then 
				az=zonef(posx,posy)
				
				if az~=nil then
					skyc=az.sky
					gndc=az.gnd
				end
			end 
			
   rectfill(x,0,x,dstart2,skyc)
   rectfill(x,dend,x,scrh,gndc)
   
   pal_swap(p)
   
   sspr(tex.t*16+texx,texl*16,
   	1,16,
   	x,dstart2,1,dend-dstart+1)
  end

  local h=scrh

  if (dend<0) dend=h

  if x%2==0 and dend<128 then
   -- store stripe data
   add(floors,{x,check%2,dend,ray.x,ray.y,
    flr(max(detlim_tbl[x],dend)),stacks})
   check+=1
  end
 end

 -- draw floor
 if settings.nofloor~=true then
	 draw_floor(floors)
 end
end

function castat(world,plr,x,scrw)
 local pos=plr.pos
 local dr=plr.dr
 local pl=plr.pl
 local camx=2*x/scrw-1
 local rayn=vec:copy(pos)

 local ray=vec:new({
   x=dr.x+pl[1]*camx,
   y=dr.y+pl[2]*camx,
  })

 local mapx=flr(pos.x)
 local mapy=flr(pos.y)

 -- ray casting
 local sdx=0
 local sdy=0
 local dx=abs(1/ray.x)
 local dy=abs(1/ray.y)

 -- ray direction stepping vars
 local sx=0
 local sy=0

 -- stop signals
 local hit=0
 local side=0
 local rxe2=0
 local rye2=0

 if ray.x<0 then
  sx=-1
  sdx=(pos.x-mapx)*dx
 elseif ray.x>0 then
  sx=1
  sdx=(mapx+1-pos.x)*dx
 else
  sdx=20000
 end

 if ray.y<0 then
  sy=-1
  sdy=(pos.y-mapy)*dy
 elseif ray.y>0 then
  sy=1
  sdy=(mapy+1-pos.y)*dy
 else
  sdy=20000
 end

 local tex=nil
 local door=nil
 while hit==0 do
  if sdx<sdy then -- horizontal advancing
   sdx+=dx
   mapx+=sx
   side=0 -- w<>e
  else
   sdy+=dy
   mapy+=sy
   side=1 -- n<>s
  end

  tex=world.map[mapy][mapx]

  if tex and tex.t>1 then
   local mapx2=mapx
   local mapy2=mapy
   if (rayn.x<mapx) mapx2-=1
   if (rayn.y>mapy) mapy2+=1

   local adj=1
   local rmul=1

   if side==1 then
    adj=mapy2-rayn.y
    rmul=adj/ray.y
   else
    adj=(mapx2-rayn.x)+1
    rmul=adj/ray.x
   end

   rxe2=rayn.x+ray.x*rmul
   rye2=rayn.y+ray.y*rmul

   if tex.d==true then
    local tdx=sqrt(1+(ray.y^2)/(ray.x^2))
    local tdy=sqrt(1+(ray.x^2)/(ray.y^2))

    if abs(ray.x)<0.01 then
     tdx=100
    end

    if abs(ray.y)<0.01 then
     tdy=100
    end

    local d=doorf(mapx,mapy)
    door=d
    if side==0 then
     local tsy=sqrt(tdx^2-1)
     local hsy=rye2+(sy*tsy)/2

     if flr(hsy)==mapy and hsy-mapy>d.oval/100
     then
      hit=1
     end
    else
     local tsx=sqrt(tdy^2-1)
     local hsx=rxe2+(sx*tsx)/2

     if flr(hsx)==mapx and hsx-mapx>d.oval/100 
     then
      hit=1
     end
    end
   else
    hit=1
   end
  end
 end

 return pos,ray,mapx,sx,mapy,sy,tex,side,rxe2,rye2,door
end

function raycast(world,plr,x,scrw)
 local pos,ray,mapx,sx,mapy,sy,tex,side,rxe2,rye2,d=castat(world,plr,x,scrw)

 local pwalld=0

 if side==0 then
  if tex.d==true then
   mapx+=sx/2
  end
  pwalld=(mapx-pos.x+(1-sx)/2)/ray.x
 else
  if tex.d==true then
   mapy+=sy/2
  end
  pwalld=(mapy-pos.y+(1-sy)/2)/ray.y
 end

 return pwalld,ray,side,tex,mapx,mapy,rxe2,rye2,d
end

function draw_floor(floors)
 pal_reset()

 local h=scrh or 128
 for f in all(floors) do
  local x=f[1]
  local check=f[2]
  local dend=f[3]
  local rdx=f[4]
  local rdy=f[5]
  local sdith=f[6]
  local stacks=f[7]

  local cdst=0
  local cfx=0
  local cfy=0
  local ftex=0
  local ftexx=0
  local ftexy=0

		if settings.nodither~=true then
	  for y=min(sdith-check,127),dend+5,-2 do
	   cdst=dist_tbl[y]
	   cfx=cdst*rdx+plr.pos.x
	   cfy=cdst*rdy+plr.pos.y
	
	   ftex=peek(0x4300+32*flr(cfy)+flr(cfx))
	
	   ftexx=(cfx*16)%16
	   ftexy=(cfy*16)%16
	   local tcol=sget(ftex*16+ftexx,ftexy)
	
	   pset(x,y,tcol)
	  end
  end

  for y=h-check,sdith,-2 do
   cdst=dist_tbl[clamp(y,0,#dist_tbl)]
   cfx=cdst*rdx+plr.pos.x
   cfy=cdst*rdy+plr.pos.y

   ftex=peek(0x4300+32*flr(cfy)+flr(cfx))
   local tmi=tex2map_map[ftex]

   if tmi and tmi.d==true then
    ftex=0 -- peek(0x4300+32*flr(cfy)+flr(cfx))
   end

   ftexx=(cfx*16)%16
   ftexy=(cfy*16)%16
   local tcol=sget(ftex*16+ftexx,ftexy)

   rectfill(x-2,y,x+1,y,tcol)

   -- if ftex==settings.doorid then
   --  local trtex=sget(ftexx,ftexy+16)
   --  rectfill(x,128-y,x+3,128-y,trcol)
   -- end
  end
 end
end

-- generators

function genworld()
 local m={}
 local d={}
 for y=0,31 do
  m[y+1]={}
 
  for x=0,127 do
   local mpd=mget(x,y)
   local tex=map2tex(mpd)

   if tex==nil then
    mset(x,y,settings.mapstart)
    tex=map2tex(mget(x,y))
   end
   
   tex={
   	t=tex.t,
   	h=tex.h or 1,
   	d=tex.d or false,
   	m=mpd,
   }

   m[y+1][x+1]=tex
   if tex.d==true then
    add(d,{pos={x=x+1,y=y+1},oval=0,vel=0})
   end
  end
 end
 return {
  map=m,
  doors=d,
 }
end

function genpal()
 for z=0,127 do
  for i=0,15 do
   local col=sget(i,z+64)
   if i==12 then
    col=bor(0x80,i)
   end
   poke(0x5000+16*z+i,col)
  end
 end
end

-- palette switches

function pal_reset()
 pal_swap(0)
end

function pal_swap(z)
 memcpy(0x5f00,0x5000+16*z,16)
end

-- player

function plr:new()
 self.pos=vec:new()
 self.angle=90
 self.dr=vec:new({x=1,y=0})
 self.pl={0,0.66}
end

function plr:setpos(x,y)
 self.pos.x=x
 self.pos.y=y
end

function plr:turn(rot)
 self.angle+=rot
 rot=rot/360

 local odrx=self.dr.x
 self.dr.x=odrx*cos( rot)-self.dr.y*sin(-rot)
 self.dr.y=odrx*sin(-rot)+self.dr.y*cos(-rot)

 local oplx=self.pl[1]
 self.pl[1]=oplx*cos(-rot)-self.pl[2]*sin(-rot)
 self.pl[2]=oplx*sin(-rot)+self.pl[2]*cos(-rot)
end

function plr:move(sp)
 local mx=self.pos.x+self.dr.x*sp
 local my=self.pos.y+self.dr.y*sp

 local msq=_world.map[flr(self.pos.y)][flr(mx)]

 if msq.t<1 or (msq.d==true and doorf(flr(mx),flr(self.pos.y)).oval>70) then
  self.pos.x=mx
 end

 msq=_world.map[flr(my)][flr(self.pos.x)]

 if msq.t<1 or (msq.d==true and doorf(flr(self.pos.x), flr(my)).oval>70) then
  self.pos.y=my
 end
end

-- door

function update_doors(world)
 for d in all(world.doors) do
  local dp=vec:copy(d.pos)
  dp:add({x=0.5,y=0.5})
  local dtd=plr.pos:dist(dp)

  if dtd<2 then
   if d.oval<100 then
    d.vel=2
   end
  else
   if d.oval>0 then
    d.vel=-2
   end
  end
  d.oval=clamp(d.oval+d.vel,0,100)
 end
end

function doorf(mapx,mapy)
 for d in all(_world.doors) do
  if d.pos.x==mapx and d.pos.y==mapy then
   return d
  end
 end
 return nil
end

-- mappings

function map2tex(val)
 return map2tex_map[val]
end

-- zones
-- an attempt to provide
-- special behavior in specific zones
-- with shading configuration and/or logic changes
function zonef(x,y)
 x=(x-1) or -1
 y=(y-1) or -1
 
	for z in all(_world.zones) do
		if z.x1+0.5<x and z.x2+0.5>x and
				 z.y1+0.5<y and z.y2+0.5>y then
			return z	 
		end 
	end
	
	if 0.5<x and 127.5>x and
				0.5<y and 127.5>y then
		return {
			x1=0,y1=0,
			x2=127,y2=127,
			sky=settings.skycolor,
			gnd=settings.groundcolor,
		}		
	end
	
	return nil
end
-->8
-- utils

-- copy tables
function copy(o)
 local c={}
 if type(o) == 'table' then
  for k,v in pairs(o) do
   c[k]=copy(v)
  end
 else
  c=o
 end
 return c
end

-- performance analysis
frames=0
function calcfps(now)
 frames+=1
 local n=flr(now)
 if (n!=lastclock) then
  fps=frames
  frames=0
  lastclock=n
 end
end

function wait(a)
 for i=1,a do flip() end
end

function clamp(x,minv,maxv)
 return min(max(x,minv), maxv)
end

-->8
-- math

function pow(x,a)
 if (a==0) return 1
 if (a <0) x,a=1/x,-a
 local r,a0,xn=1,flr(a),x
 a-=a0
 while a0>=1 do
  if (a0%2>=1) r*=xn
  xn,a0=xn*xn,shr(a0,1)
 end
 while a>0 do
  while a<1 do x,a=sqrt(x),a+a end
  r,a=r*x,a-1
 end
 return r
end

function lerp(a,b,t)
 return a*(1-t)+b*t
end

function unlerp(t,a,b)
 return (t-a)/(b-a)
end

function step(a,b,t)
 local x=unlerp(t,a,b)
 return (x^2)*(3-2*x)
end

function step2(a,b,t)
 local x=unlerp(t,a,b)
 return (x^3)*(x*(6*x-15)+10)
end

function scale(t,a,b,c,d)
	return (b-a)*(t-c)/(d-c)+a
end

-- goniometric extensions

function acos(x)
 return atan2(x, -sqrt(1-x^2))
end

function asin(y)
 return atan2(sqrt(1-y^2),-y)
end

function deg(ang01)
 return ang01*360
end

function ang01(deg)
 return deg/360
end

-- vector
vec={x=0,y=0,z=0}

function vec:new(o)
 self.__index = self
 return setmetatable(o or {}, self)
end

function vec:copy(v)
 local r=vec:new()
 v=sanvec(v)
 r.x=v.x
 r.y=v.y
 r.z=v.z
 return r
end

function sanvec(v,d)
 d=d or vec:new()
 v.x=v.x or d.x
 v.y=v.y or d.y
 v.z=v.z or d.z
 return v
end

function vec:str()
 return "x: "..self.x.." y: "..self.y.." z: "..self.z
end

function vec:add(v)
 v=sanvec(v)
 self.x+=v.x
 self.y+=v.y
 self.z+=v.z
end

function vec:sub(v)
 v=sanvec(v)
 self.x-=v.x
 self.y-=v.y
 self.z-=v.z
end

function vec:mul(v)
 v=sanvec(v)
 self.x*=v.x
 self.y*=v.y
 self.z*=v.z
end

function vec:div(v)
 v=sanvec(v)
 self.x/=v
 self.y/=v
 self.z/=v
end

function vec:cross(v)
 v=sanvec(v)
 local c=vec:new()
 c.x=self.y*v.y
 c.y=self.z*v.z
 c.z=self.x*v.x
 return c
end

function vec:cmp(v)
 v=sanvec(v,self)
 if self.x==v.x and self.y==v.y and self.z==v.z then
  return 1
 end
 return _
end

function vec:dot(v)
 return self.x*v.x+self.y*v.y+self.z*v.z
end

function vec:mag2()
 return self.x^2+self.y^2+self.z^2
end

function vec:mag()
 return sqrt(self:mag2())
end

function vec:dist2(v)
 local c=vec:copy(self)
 c:sub(v)
 return c:mag2()
end

function vec:dist(v)
 return sqrt(self:dist2(v))
end

function vec:norm()
 local w=self:mag()
 self.x/=w
 self.y/=w
 self.z/=w
end

function vec:angle(v)
 local p=self:dot(v)
 local l=self:mag()*v:mag()
 return acos(p/l)
end

function vec:lerp(a,b,t)
 local c=vec:new()
 c.x=lerp(a.x,b.x,t)
 c.y=lerp(a.y,b.y,t)
 c.z=lerp(a.z,b.z,t)
 return c
end
__gfx__
ddddddd5dddddddd5111511311b1153b442444342244422411111111111111111122211111122211112d3111111222115d5d515d5d5d5d5d1111111111111111
dd5d6dddddd5ddd51311131b1131113154452444443444541a92a92a95a92a91411111442211511441d3b344221151141111111111111111176d76d76576d761
dddddddddddddddd1331111b1131111144522442444224441a459459449459412211142222211142221b3422222111425d6761111d6767151755655655665651
dddddddd6dddd6dd1b31511b511513114422134454421444194244544244244122211222222211222223b222222211225dddd155ddddd615165d55555d55d551
d5ddd5dddddddddd13b3111b1113135144444434524454431444944944944941252112225552112225213222525211225d5d51555d5d55151555655655655651
dddddddddd5ddd5d1b33131313311b1142245444212442241442452442442441551115555551111252111525252111121115511111151151155d55d55d55d551
5d6ddddddddddddd1b3b111331311b11212244442224212412222222222222211111111111111111111111111111111111511111111111111dddddddddddddd1
dddd5ddd5ddddddd1b3315111111131122144124444252141424424524254241114422111442221111442211143d3311551155d66767155615d55d55d5d55d51
d5dddddddddd6d5d13b31113115313514444422444444444141dd444444444411422221442222211142222144223b2115511555555561555151ff55555555551
ddddddd6dddddddd1b3135131511111124444444454224341211222222222221122225122222211112222512222b3111d51ddddddd5515d51d11ddddddddddd1
ddddd5dddddddddd131b31b11111515112442444244114421111111111111111112551122222114411252112222231445d11ddddddd5115d1111111111111111
dd5dddddd5ddddd5151331b5311111312243442124422442122122122122122121111122525114222111112252511422d51111111d5511d51dd1dd1dd1dd1dd1
d6dddd6dddd5dddd111513313151113124443412444444241441442552542421511111255511122251133125251112221111111151111111155155d55d55d5d1
dddddddddddddddd51111111111131b34424424444344444141154144144112111442112511411521143b11251141152555d6d6711555d5515111515115511d1
5d5ddddd5ddddddd115311151131313b52124434224442241111211211121111142222111142211114223211114221115555d5d61155d6d51111d11d111d1111
dddd5dddddd6d5d61113311111b1113b41225442212452221111111111111111122222111122221112222211112222111111111511111d6d1111111111111111
11111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
aa92a92a95a92a991a92a92a95a92a91000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9a459459449459441a45422222245941000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
494244544244244419422c1cc1c22441000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
444494494494494414442c5cc5c24941000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
44424524424424441442251551522441000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
222222222222222212222c5cc5c22221000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
452542452425425414242c5cc5c24241000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
444444444444444414141c5cc5c14441000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
222222222222222212111c1cc1c12221000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
22212212212212221221221221221221000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
44414425525424241441441241242421000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
44115414414411241411541241241121000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
41112112111211141111211211121111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0123456789abcdef0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
012325562493dde90077000770070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2123255d2493d5240070707007070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0111215d2243d5240070707777070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01111115122115220077007007070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00011111122111220070007007070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00001111122111220070007007077700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00001011111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000001000000000007700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000077700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000777700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000777777777777700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000777700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000077700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000007700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
7555555251115115442444341111111111111111111111111111111111111111111111119249242a000000000000000000000000000000000000000000000000
5d5ddd5515b335515445244419a29a5111521141115211411d1d1dd1167d6751199449219249a444000000000000000000000000000000000000000000000000
5dddddd51333b33144522442149549411211142112113b211d6761d1156556511acc5c9122222222000000000000000000000000000000000000000000000000
5d6666d513baab31242213441442442112411221124132211dddd151155d55d11a66554152452425000000000000000000000000000000000000000000000000
556665d5133abab1444444421494494115211221152112211d5d51511565565114cc6c4144444444000000000000000000000000000000000000000000000000
5dd66dd513b333314224544214424421152154511521545111155111155d55d114cc5c9111221111000000000000000000000000000000000000000000000000
5dddddd51b3b3b512142444412222221151155111511551111511d611dddddd11249a2a141441424000000000000000000000000000000000000000000000000
55555555111515114214412411111111111111111111111111111111111111111111111141442414000000000000000000000000000000000000000000000000
__label__
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
d55dd55dd55dd55d55cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc555dd55d551155dd55dd5d
d55dd55dd55dd55d5555cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc55555dd55d551155dd55dd5d
d55dd55dd55dd55d515555cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc15551111111111111111111111
1111111111111111111155555ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc5515111111111111111111111111
111111111111111111111155555cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc5555511111111111111111111111111
111dd66776677115551111111555ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc555111111555dd6676611111111dd67
111dd66776677115555d11111115ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc1111111d5555dd6676611111111dd67
111dd66776677115555d51155111ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc1111115d5555ddddddd115555dddddd
5dddddddddd66115555551155511cccccc55d5d5155d5d5d55d5d5d55d515d55d5d5d55d5d5d5115d5d5dd5d5dccccccc155111555555ddddddd115555dddddd
5dddddddddd66115555555155515cccccc55d5d5155d5d5d55d5d5d55d515d55d5d5d55d5d5d5115d5d5dd5d5dccccccc155511555555ddddddd115555dddddd
555dd55dd5555115555555555515cccccc11111111111111111111111111111111111111111111111111111111ccccccc155551555555dd55d5511555555dd5d
555dd55dd5555115555555555515cccccc55d67611111d67667155d667611111d67671155d67611111d6776715ccccccc155551555555dd55d5511555555dd5d
555dd55dd5555115515555555515cccccc55d67611111d67667155d667611111d67671155d67611111d6776715ccccccc1555515551111111555111111111115
1111111551111551111551551515cccccc55dddd1555dddddd6155ddddd155dddddd61155dddd1155dddddd615ccccccc1551515511111111555111111111115
111111155111155111155111115515111155d5d515555d5d555155d55d515555d5d551155d5d511555d5dd551555555155111115511111111555111111111115
111111111111111111115111115111111155d5d515555d5d555155d55d515555d5d551155d5d511555d5dd551555155115111115111111155111111111111111
11111111111111111151111111115515151111551111111511151111155111111151155111155111111155115151155111111111111111155111111111111111
1111111111111111155115115111151111111511111111111111111551111111111111111151111111111111111111111111515111555551115555dd66667767
66677667711555565511555d5151111111111511111111111111111551111111111111111151111111111111111111111515555515555551115555dd66667767
66677667711555565511555d51551111115551155dd66767115565511155d666767155565511555d66767715565111111515555515555551115555dd66667767
66677667711555565511555d51551151515551155dd66767115565511155d666767155565511555d667677155611115555155555155555511155555555555556
55555556611555555511555551551151115551155555555611555551115555555561555555115555555566155511551555155555155555511155555555555556
5555555661155555551555555155155111dd51dddddddd55115d5d511dddddddd5515dd5d51dddddddd55515d511111115155555555dd5511ddddddddddddd55
ddddd55551155dd5551555555155111111dd51dddddddd55115d5d511dddddddd5515dd5d51dddddddd55515d515115555155555555dd5511ddddddddddddd55
ddddd55551155dd555115555515511111155d11dddddddd51115d5d111dddddddd51155d5d11dddddddd55115d1551111515555515555dd111ddddddddddddd5
ddddd55551155dd5551155555155515333dd511111111d55111d5d5111111111d5511dd5d511111111d55511d53333333515555515555dd111ddddddddddddd5
ddddddd55111155d551151555155111333dd511111111d55111d5d5111111111d5511dd5d511111111d55511d53333333515151515555dd111ddddddddddddd5
ddddddd55111155d5511111551153333331111111111511111111111111111551111111111111111151111111133333331151111155dd551111111111111dd55
111dd55551111dd55511111511113333335555d6d667115555d555555d6d67111555d555555d6dd67115555d5533333331155111155dd551111111111111dd55
111dd55551111dd55111111111513333335555d6d667115555d555555d6d67111555d555555d6dd67115555d5533333335515111151111111111111111551111
111dd55551111dd551111511515533333355555d5dd61155dd6d555555d5d611155d6dd55555d55d611555d6d533333335511151111111111111111111551111
1551111111111111111515d155551333131111111115111111d6d111111115111111d66d111111115111111d6d13331315551d51511111111111111111551111
1551111111111111151555d155553313331111111115111111d6d111111115111111d66d111111115111111d6d33333335551d55515555555d66dd6677111155
71111555555dd555555555d155551333133333333333333333333333333333333333333333333333333333333333333315151d55555555555d66dd6677111155
71111555555dd55555555551155533133333333333333333333333333333333333333333333333333333333333331333351515555555555555dd55dd66111155
71111555555dd5555555515111351333b333333333333333333333333333333333333333333333333333333333133313533115155555555555dd55dd66111155
611115555dd66dd555515151313333133333333333333333333333333333333333333333333333333333333333331333333135151555555555dd55dd66111155
611115555dd66dd55151135133331333331333533333133313331333133333331333333313331333533313333313333313333531151111111111111155111111
51111111111dd66d1113133333333333333313331333331333333313333333133313333333133333331333135333533333333331311111111111111155111111
51111111111dd66d131333333333333333b333133333533313333333b333333333331333133313333333b3333333333313333333313333333333333333333333
51111111111dd66d133333333313331333331333133333b333133313331333133313331333b333b3331333131333133333133333333333333333333333333333
33333333333333333333333333333333131333133333133313333333b333333313331333133313331333333333b3333313331333333333333333333333333333
33333333333333333333333313133333333313331313331333133333331333333313331333133313333333131333333333133313333333333333333333333333
33333333333333333333333333331333131333133333133313331333333313335333133313331333133313333313333313331333333333333333333333333333
333333333333333333b333331333331333331333531333b333b333b333333333333333533313331333b333b3b333333333133313b33333133313333313333313
33331313331313331333335333335333133333133333133313331333333353335333133333331333133333333313333313331333331333331333131333331333
33333333133333133313133313333333333313333353331333533313331333333313333333133333331333131333333333b333331333b3333313333333333313
333313333333b333333333b3333313331313335311111111333333333333bbbb333311111111555511111111333333b333333333333333331333135333131333
13333333b33333b333b33333b33333133311111111333333333333bbbbbbbbbbbb33331111111111111111111111113333133313133313133313333313333353
3333131333131333133333131111111111111111555511111111111111111111111111111111bbbbbbbbbbbb1111111111111111331333331333131333b3b333
131333331333331333111155553333bbbb3333333333331111555511111111555555553333bbbb33333333333311115555111111115555133353333333333333
333333b333b33333111111111111111111115555111111113333bbbbbbbb3333111111111111111111115555111133333333bbbb333333331111131333131333
13133311111111111155551111333333331111111111111111111111111111111155551111111133331111111111111111111111111111111155551111333333
11111111111133331111111111111111111111111111555533331111111133331111111111111111111111111111111155551111111111113333111111111111
1111111111555511111111555511113333bbbb33333333111133333333111155551111111155551111bbbbbbbb33333333111133333333111111111111111155
33331111555511111111555511113333bbbb11111111111111113333111111115555111111111111111111111111111111111111333311111111555511111111
11111111115555111111111111111111111111111111111111333311111111111111113333333311111111111111111111555511111111333311111111111133
11111111111111113333333311111111111111111111555511111111111111111111111133333333111111111111111111115555111111111111333333331111
1133333333111155551111111155551111bbbbbbbbbbbbbbbb33333333111133333333555511111111555511113333bbbbbbbbbbbb3333333311113333333311
33333333111155551111111155551111111111111111111111111111111111111111bbbbbbbb3333111111111111111111111111111111111111111111111111
111111bbbbbbbb33331111111111111111111111111111111111111111111111111111bbbbbbbb11111111111111111111111111111111333311115555111155
11115555111111113333111111111111333311111111111133331111555511115555111111113333111111111111111133331111111111113333111155551111
11555511111111333311111111111111113333111111111111333311111111111133333333333311111111111133335555111111115555111111111111111111
33333333333311111111111133335555111111115555111111111111111111113333333333331111111111113333555511111111555511111111111111111111
333333333311111111111133335555111111111111111133333333bbbb333333331111bbbb33335555111133333333111111111111111133333333bbbb333333
bbbb333355551111111133333333111111111111111133333333bbbb333333331111bbbb333355555555111133333333111111111111111133333333bbbb3333
bbbbbb333355551111333333331111111133333333bbbb3333333333333333bbbb333311111111111111115555111111111111111133333333bbbb3333333333
11111111111111115555111111111111111133333333bbbb333333333333bbbb3333111111111111111111115555111111111111333333333333bbbb33333333
111111111111115555111111111111111133333333bbbbbbbbbbbbbbbbbbbb3333bbbb3333555511111111111111111111111133333333bbbb3333bbbbbbbbbb
555511111111111111111111111133333333bbbbbbbb3333bbbbbbbbbbbb3333bbbb33333333555511111111111111111111111133333333bbbb3333bbbbbbbb
55111111111111111111111111333333333333bbbb3333bbbbbbbbbbbbbbbb3333bbbb3333555511111111555511111111555511111111111111111111111111
11111111555511111111555555551111111111111111111111111111111111111111111111111111111155551111111111115555111111111111111111111111
11111155551111111111115555111111111111111111111111111111111111111111111111111111111111555511111111555555551111111111111111111111
11115555555511111111555511111111111111111111111111111111111111111111111111111111111133333333bbbbbbbbbbbb111111111111111111111111
1111113333bbbbbbbbbbbbbbbb1111111111111111111111111111111111111111111111111111111111113333bbbbbbbbbbbbbbbb1111111111111111111111
11113333bbbbbbbbbbbbbbbb11111111111111111111111111111111111111111111111111111111111111113333bbbbbbbbbbbbbbbb11111111111111111111
113333bbbbbbbbbbbbbbbb1111111111111111555555551111111111111111555511115555555533333333bbbbbbbb3333333333333333333311111111111155
bbbbbbbb33333333333333333333111111111111555511111111111111115555111111115555333333333333bbbb333333333333333333333333111111115555
bbbbbb33333333333333333333111111111111555511111111111111111111555511115555555533333333bbbbbbbb3333333333333333333311111111111155
bbbb33333333333333333333333311111111555555551111bbbbbbbb333333331111111111111111111111111111111111111111555555551111111133333333
111111111111115555555511111111333333333333bbbbbbbbbbbb33333333333311111111111111111111111111111111111111115555111111111111333333
11111111111111115555111111111111333333333333bbbbbbbbbbbb333333333333111111111111111111111111111111111111111155551111111111113333
111111111111115555111111111111333333333333bbbbbbbbbbbb33333333333311111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111155555555111111113333333333331111111111111111111111111111

__map__
c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c4c4c4c4c4c4c4000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c2
c6c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c6c2c2c2c2c2c2c400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c6c1c9c9c9c9c9c9c1c1c1c1c6c1c1c1c1c1c1c1c6c2c2c2c2c2c2c400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c6c1c9c0c0c0c0c9c1c1c1c1c1c1c1c1c1c1c1c1c6c2c2c2c2c2c2c400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c6c1c9c0c0c0c0c3c1c1c1c1c1c1c1c1c1c1c1c1c6c2c2c2c2c2c2c400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c6c1c9c0c0c0c0c9c1c1c1c1c1c1c1c1c1c1c1c1c6c2c2c2c2c2c2c400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c6c1c9c9c9c8c9c9c1c1c1c1c6c1c1c1c1c1c1c1c6c2c2c2c2c2c2c400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c6c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c6c2c2c2c2c2c2c400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c6c6c6c6c6c6c6c6c6c6c7c6c6c6c1c1c1c1c1c1c6c2c2c2c2c2c2c400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c6c0c2c2c2c2c2c2c2c2c2c2c2c6c1c6c1c1c6c1c6c2c2c2c2c2c2c500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c6c0c1c1c1c1c1c1c1c1c2c2c2c6c1c1c1c1c1c1c6c2c2c2c2c2c2c500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c6c0c1c1c1c1c1c1c1c1c2c2c2c6c1c1c1c1c1c1c6c2c2c2c2c2c2c500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c6c0c0c0c0c0c0c0c0c0c0c0c0c7c1c1c1c1c1c1c7c2c2c2c2c2c2c400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c6c0c1c1c1c1c1c1c1c1c2c2c2c6c1c6c1c1c6c1c6c2c2c2c2c2c2c400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c6c0c1c1c1c1c1c1c1c1c2c2c2c6c1c1c1c1c1c1c6c2c2c2c2c2c2c400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c6c6c6c2c2c2c2c2c2c2c2c6c6c6c6c6c6c6c6c6c6c4c4c5c5c4c4c400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000c6c6c6c6c6c6c6c6c6000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0126000010775137751777513775107751377517775137750f7751377517775137750f77513775177751377500000000000000000000000000000000000000000000000000000000000000000000000000000000
011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
03 01424344

