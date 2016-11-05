fabric.Polygon.prototype.GetGlobalPNTS = function() {
		 /*создаём точку с координатами [0,0], она константна, далее используем 
		 её как один из параметров метода определённого в fabric.util, вероятно есть 
		 более симпатичный способ принятый в js но разбираться лень*/
		const ZPoint = new fabric.Point(0,0);
		
		/*сюда мы будем записывать координаты вершин вычисленные относительно origin канваса,
		у нас это будет всегда верхний левый угол
		*/
		var MyPoints=new Array(0);

		/*для всех точек лежащих в свойстве points данного многоугольника пересчитываем координаты*/
		for (var i = 0, len = this.points.length; i < len; i++) {
			/*создаём временную переменную для записи новых координат i-й точки*/
			var MyPoint=new fabric.Point;
			/*пишем в эту переменную координаты i-й точки многоугольника*/
			MyPoint.setFromPoint(this.points[i]);
			/*считаем для нашей точки новые координаты исходя из scale данного многоугольника*/
			MyPoint.x*=this.scaleX;
			MyPoint.y*=this.scaleY;
			/*считаем для нашей точки новые координаты исходя из угла на который повернут многоугольник
			фактически мы поворачиваем эту точку на заданный угол относительно центра вращения всей фигуры
			т.к. все координаты вершин заданы относительно origin многоугольника, координаты центра его
			вращения в его системе координат-[0,0]. Это утверждение справедливо когда центр вращения совпадает
			с origin фигуры.
			*/
			MyPoint=fabric.util.rotatePoint(MyPoint,ZPoint,fabric.util.degreesToRadians(this.angle));
			/*Теперь мы знаем точные координаты точки относительно центра фигуры, после применения масштабирования и вращения. Чтобы
			посчитать абсолютную позицию (относительно origin канваса), нужно прибавить к координатам к коорднатам MyPoint
			прибавить соответствующие координаты центра фигуры вычисленные относительно канваса. Делаем это методом addEquals.
			*/
			MyPoint.addEquals(this.getCenterPoint());
			/*Пишем новую точку в массив и повтолряем всё для следующей точки*/
			MyPoints.push(MyPoint);
		}
		return MyPoints;
	  }
fabric.Polygon.prototype.getRDisp = function(){return this.GetGlobalPNTS().map(function(element){return [element.x,element.y]})};

fabric.Ellipse.prototype.GetGlobalPNTS = function() {
		dataList={
			CX: this.getCenterPoint().x,
			CY: this.getCenterPoint().y,
			MajAxis: this.rx*this.scaleX,
			MinAxis: this.ry*this.scaleY,
			Angle: this.angle
		}
		return dataList;
	  }
fabric.Ellipse.prototype.getRDisp = function(){return this.GetGlobalPNTS()};


fabric.Canvas.prototype.reloadBG = function(imgToSet){
		this.setBackgroundImage(imgToSet.src,this.renderAll.bind(this));
		this.setWidth(imgToSet.width);
		this.setHeight(imgToSet.height);
};
	
	
	
fabric.Object.prototype.hide = function(){this.set({opacity:0,selectable:false,hidden:true,active:false})}
fabric.Object.prototype.show = function(){this.set({opacity:0.5,selectable:true,hidden:false,active:true,focus:true})}
fabric.Object.prototype.toggleVisability = function(){if(this.hidden){this.show()}else{this.hide();this.sendBackwards()}}
fabric.Object.prototype.hidden = false;
	
	